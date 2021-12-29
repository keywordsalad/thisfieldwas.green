module Green.Template.Context where

import Control.Monad.State.Strict
import Data.Aeson
import Data.Bifunctor
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Proxy
import Data.Scientific (isInteger, toBoundedInteger, toBoundedRealFloat)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Green.Common
import Green.Template.Ast
import Prelude hiding (lookup)

newtype Context a = Context {unContext :: ContextFunction a}

type ContextFunction a = String -> TemplateRunner a (ContextValue a)

getContext :: Identifier -> Compiler (Context a)
getContext id' = intoContext <$> getMetadata id'

itemFilePath :: Item a -> FilePath
itemFilePath = toFilePath . itemIdentifier

data TemplateState a = TemplateState
  { tplContextStack :: [Context a],
    tplItemStack :: [Item a],
    tplCallStack :: [String]
  }

type TemplateRunner a b = StateT (TemplateState a) Compiler b

tplItem :: TemplateRunner a (Item a)
tplItem = gets $ head . tplItemStack

tplModifyItem :: (Item a -> TemplateRunner a (Item a)) -> TemplateRunner a ()
tplModifyItem f =
  tplItem
    >>= f
    >>= tplReplaceItem

tplReplaceItem :: Item a -> TemplateRunner a ()
tplReplaceItem item = do
  stack <-
    gets tplItemStack <&> \case
      [] -> [item]
      _ : rest -> item : rest
  modify \s -> s {tplItemStack = stack}

tplPopItem :: TemplateRunner a (Item a)
tplPopItem =
  gets tplItemStack >>= \case
    [] -> error "tplPopItem: empty stack"
    x : xs -> do
      modify \s -> s {tplItemStack = xs}
      return x

tplPopBody :: TemplateRunner a a
tplPopBody = itemBody <$> tplPopItem

tplPushItem :: Item a -> TemplateRunner a ()
tplPushItem item = do
  stack <- gets tplItemStack
  modify \s -> s {tplItemStack = item : stack}

tplContext :: TemplateRunner a (Context a)
tplContext = gets $ head . tplContextStack

tplPushContext :: Context a -> TemplateRunner a ()
tplPushContext context =
  modify \s -> s {tplContextStack = context : tplContextStack s}

tplWithItem :: Item a -> TemplateRunner a b -> TemplateRunner a b
tplWithItem item f = do
  stack <- gets tplItemStack
  modify' \s -> s {tplItemStack = item : stack}
  x <- f
  modify' \s -> s {tplItemStack = stack}
  return x

-- | Place context within a given scope.
tplWithContext :: Context a -> TemplateRunner a b -> TemplateRunner a b
tplWithContext context f =
  gets tplContextStack >>= \stack -> do
    let head' = case stack of
          (next : _) -> context <> next
          [] -> context
    -- temporarily push the new context onto the stack
    modify' \s -> s {tplContextStack = head' : stack}
    x <- f
    -- pop the new context off the stack
    modify' \s -> s {tplContextStack = stack}
    return x

-- | Place context in global scope.
tplPut :: Context a -> TemplateRunner a ()
tplPut context = do
  contextStack <- gets tplContextStack
  let init' = init contextStack
      last' = last contextStack
  -- prepend the new context to the root context
  modify' \s -> s {tplContextStack = init' <> [context <> last']}
  return ()

tplWithCall :: String -> TemplateRunner a b -> TemplateRunner a b
tplWithCall call f = do
  stack <- gets tplCallStack
  modify' \s -> s {tplCallStack = call : stack}
  x <- f
  modify' \s -> s {tplCallStack = stack}
  return x

tplWithField :: String -> TemplateRunner a b -> TemplateRunner a b
tplWithField field' f = do
  file <- itemFilePath <$> tplItem
  tplWithCall (show field' ++ " in " ++ file) f

tplFail :: String -> TemplateRunner a b
tplFail message = fail =<< tplTraced message

tplTried :: String -> TemplateRunner a b
tplTried = lift . noResult

tplTrace :: TemplateRunner a [String]
tplTrace = gets tplCallStack

tplTraced :: String -> TemplateRunner a String
tplTraced message = do
  trace <- tplTrace
  return $ message ++ ", trace: [" ++ intercalate ", " trace ++ "]"

-- | Apply @f@ to an item if @key@ is requested.
field :: (IntoValue v a) => String -> (Item a -> TemplateRunner a v) -> Context a
field key f = Context f'
  where
    f' k
      | k == key = tplWithField k do
        i <- tplItem
        intoValue <$> f i
      | otherwise = lift . noResult $ "tried " ++ show key

-- | Reports missing field.
missingField :: Context a
missingField = Context f
  where
    f key = lift . noResult $ "missing " ++ show key

-- | Const-valued field, returns the same @val@ per @key@.
constField :: (IntoValue v a) => String -> v -> Context a
constField key val = field key f
  where
    constResult = return val
    f _ = constResult

-- | Mapping of function @g@ after context @f@.
mapField :: (FromValue v a, IntoValue w a) => (v -> w) -> Context a -> Context a
mapField g (Context f) = Context h
  where
    h k = tplWithCall ("mapField of " ++ show k) do
      fmap (intoValue . g) $ fromValue =<< f k

-- | Binding of function @g@ after context @f@.
bindField :: (FromValue v a, IntoValue w a) => (v -> TemplateRunner a w) -> Context a -> Context a
bindField g (Context f) = Context h
  where
    h k = do
      tplWithCall ("bindField of " ++ show k) do
        fmap intoValue $ g =<< fromValue =<< f k

-- | Alternation of context @g@ after context @f@.
composeField :: Context a -> Context a -> Context a
composeField (Context g) (Context f) = Context h
  where
    h name = do
      s <- get
      lift $ evalStateT (f name) s <|> evalStateT (g name) s

-- | Lookup of @val@ by @key@ into provided @HashMap@.
hashMapField :: (IntoValue v a) => HashMap String v -> Context a
hashMapField m = Context f
  where
    m' = intoValue <$> m
    f k = maybe tried return (HashMap.lookup k m')
    tried = lift . noResult $ "tried hashmap of " ++ show (HashMap.keys m')

forItemField :: (IntoValue v a) => String -> [Identifier] -> (Item a -> TemplateRunner a v) -> Context a
forItemField key ids f = field key f'
  where
    f' item
      | itemIdentifier item `elem` ids = f item
      | otherwise = tplTried $ show key ++ " for items " ++ show (toFilePath <$> ids)

functionField :: (FromValue v a, IntoValue w a) => String -> (v -> TemplateRunner a w) -> Context a
functionField = constField

functionField2 :: (FromValue v a, FromValue x a, IntoValue w a) => String -> (v -> x -> TemplateRunner a w) -> Context a
functionField2 = constField

functionField3 :: (FromValue v a, FromValue x a, FromValue y a, IntoValue w a) => String -> (v -> x -> y -> TemplateRunner a w) -> Context a
functionField3 = constField

functionField4 :: (FromValue v a, FromValue x a, FromValue y a, FromValue z a, IntoValue w a) => String -> (v -> x -> y -> z -> TemplateRunner a w) -> Context a
functionField4 = constField

instance Semigroup (Context a) where
  (<>) = flip composeField

instance Monoid (Context a) where
  mempty = missingField

class IntoContext v a where
  intoContext :: v -> Context a

instance IntoContext (Context a) a where
  intoContext = id

instance (IntoValue v a) => IntoContext (HashMap String v) a where
  intoContext = hashMapField

instance (IntoValue v a) => IntoContext [(String, v)] a where
  intoContext = intoContext . HashMap.fromList

instance IntoContext Object a where
  intoContext = ic . fmap (bimap T.unpack intoValue) . HashMap.toList
    where
      ic :: [(String, ContextValue a)] -> Context a
      ic = intoContext

data ContextValue a
  = EmptyValue
  | UndefinedValue String (Item a) [String] [String]
  | ContextValue (Context a)
  | ListValue [ContextValue a]
  | BoolValue Bool
  | StringValue String
  | DoubleValue Double
  | IntValue Int
  | FunctionValue (ContextValue a -> TemplateRunner a (ContextValue a))
  | BlockValue Block
  | ItemValue (Context a) [Item a]
  | ThunkValue (TemplateRunner a (ContextValue a))

type FunctionValue v w a = v -> TemplateRunner a w

type FunctionValue2 v x w a = v -> FunctionValue x w a

type FunctionValue3 v x y w a = v -> FunctionValue2 x y w a

type FunctionValue4 v x y z w a = v -> FunctionValue3 x y z w a

instance Show (ContextValue a) where
  show = \case
    EmptyValue -> "EmptyValue"
    UndefinedValue name item trace errors -> "UndefinedValue " ++ show name ++ " in item context for " ++ itemFilePath item ++ ", trace=[" ++ intercalate ", " trace ++ "], suppressed=[" ++ intercalate ", " errors ++ "]"
    ContextValue {} -> "ContextValue"
    ListValue values -> "ListValue " ++ show values
    BoolValue value -> "BoolValue " ++ show value
    StringValue value -> "StringValue " ++ show value
    DoubleValue value -> "DoubleValue " ++ show value
    IntValue value -> "IntValue " ++ show value
    FunctionValue {} -> "FunctionValue"
    BlockValue {} -> "BlockValue"
    ItemValue _ items -> "ItemValue " ++ show (itemFilePath <$> items)
    ThunkValue {} -> "ThunkValue"

itemValue :: Context a -> Item a -> ContextValue a
itemValue context item = intoValue (context, [item])

itemListValue :: Context a -> [Item a] -> ContextValue a
itemListValue context items = intoValue (context, items)

class IntoValue' (flag :: Bool) v a where
  intoValue' :: Proxy flag -> v -> ContextValue a

-- "Specialize" List
type family FString a :: Bool where
  FString Char = 'True
  FString _ = 'False

-- | Inject a concrete type @v@ into a @ContextValue a@.
class IntoValue v a where
  intoValue :: v -> ContextValue a

instance (FString v ~ flag, IntoValue' flag [v] a) => IntoValue [v] a where
  intoValue = intoValue' (Proxy :: Proxy flag)

instance (IntoValue v a) => IntoValue' 'False [v] a where
  intoValue' _ = ListValue . (intoValue <$>)

instance IntoValue' 'True String a where
  intoValue' _ = StringValue

instance IntoValue Block a where
  intoValue = BlockValue

instance IntoValue (ContextValue a) a where
  intoValue = id

instance IntoValue (Context a) a where
  intoValue = ContextValue

instance IntoValue Value a where
  intoValue = \case
    Object o -> ContextValue $ intoContext o
    Array a -> ListValue $ Vector.toList $ Vector.map intoValue a
    String t -> StringValue $ T.unpack t
    Number n
      | isInteger n -> IntValue $ fromJust $ toBoundedInteger n
      | otherwise -> DoubleValue $ fromRight 0.0 $ toBoundedRealFloat n
    Bool b -> BoolValue b
    Null -> EmptyValue

instance IntoValue () a where
  intoValue () = EmptyValue

instance IntoValue Bool a where
  intoValue = BoolValue

instance IntoValue Double a where
  intoValue = DoubleValue

instance IntoValue Int a where
  intoValue = IntValue

instance IntoValue (Context a, [Item a]) a where
  intoValue = uncurry ItemValue

instance (IntoValue v a) => IntoValue (Maybe v) a where
  intoValue (Just v) = intoValue v
  intoValue Nothing = EmptyValue

instance (FromValue v a, IntoValue w a) => IntoValue (FunctionValue v w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv = do
        v <- fromValue cv
        intoValue <$> f v

instance (FromValue v a, FromValue x a, IntoValue w a) => IntoValue (FunctionValue2 v x w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv =
        intoValue . f <$> fromValue cv

instance (FromValue v a, FromValue x a, FromValue y a, IntoValue w a) => IntoValue (FunctionValue3 v x y w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv =
        intoValue . f <$> fromValue cv

instance (FromValue v a, FromValue x a, FromValue y a, FromValue z a, IntoValue w a) => IntoValue (FunctionValue4 v x y z w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv =
        intoValue . f <$> fromValue cv

instance IntoValue (TemplateRunner a (ContextValue a)) a where
  intoValue = ThunkValue

-- | Extract a concrete value of type @v@ from a @ContextValue a@.
class FromValue v a where
  fromValue :: ContextValue a -> TemplateRunner a v

class FromValue' (flag :: Bool) v a where
  fromValue' :: Proxy flag -> ContextValue a -> TemplateRunner a v

instance (FString v ~ flag, FromValue' flag [v] a) => FromValue [v] a where
  fromValue = fromValue' (Proxy :: Proxy flag)

instance (FromValue v a) => FromValue' 'False [v] a where
  fromValue' flag = \case
    ListValue xs -> sequence $ fromValue <$> xs
    ThunkValue fx -> fromValue' flag =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as List"

instance FromValue' 'True String a where
  fromValue' flag = \case
    StringValue x -> return x
    ThunkValue fx -> fromValue' flag =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as String"

instance FromValue (Context a) a where
  fromValue = \case
    ContextValue c -> return c
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Context"

instance FromValue (ContextValue a) a where
  fromValue = return

instance FromValue Bool a where
  fromValue = \case
    BoolValue x -> return x
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Bool"

instance FromValue Double a where
  fromValue = \case
    DoubleValue x -> return x
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Double"

instance FromValue Int a where
  fromValue = \case
    IntValue x -> return x
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Int"

instance FromValue (Context a, [Item a]) a where
  fromValue = \case
    ItemValue context items -> return (context, items)
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Item"

instance FromValue Block a where
  fromValue = \case
    BlockValue block -> return block
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Block"

instance (IntoValue v a, FromValue w a) => FromValue (FunctionValue v w a) a where
  fromValue cv = case cv of
    FunctionValue f -> return f'
      where
        f' v = fromValue =<< f (intoValue v)
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Function"

instance (IntoValue v a, IntoValue x a, FromValue w a) => FromValue (FunctionValue2 v x w a) a where
  fromValue cv = case cv of
    FunctionValue f -> return f'
      where
        f' v x = do
          g <- fromValue =<< f (intoValue v)
          g x
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Function2"

instance (IntoValue v a, IntoValue x a, IntoValue y a, FromValue w a) => FromValue (FunctionValue3 v x y w a) a where
  fromValue = \case
    FunctionValue f -> return f'
      where
        f' v x y = do
          g <- fromValue =<< f (intoValue v)
          h <- g x
          h y
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Function3"

instance (IntoValue v a, IntoValue x a, IntoValue y a, IntoValue z a, FromValue w a) => FromValue (FunctionValue4 v x y z w a) a where
  fromValue = \case
    FunctionValue f -> return f'
      where
        f' v x y z = do
          g <- fromValue =<< f (intoValue v)
          h <- g x
          i <- h y
          i z
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Function4"
