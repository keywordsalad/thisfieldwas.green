module Green.Template.Context where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
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
  { tplContextStack :: [(Context a, Context a)],
    tplItemStack :: [Item a],
    tplCallStack :: [String]
  }

type TemplateRunner a b = StateT (TemplateState a) Compiler b

tplItem :: TemplateRunner a (Item a)
tplItem =
  gets tplItemStack >>= \case
    [] -> tplFail "tplItem: no Item on stack"
    (item : _) -> return item

tplModifyItem :: (Item a -> TemplateRunner a (Item a)) -> TemplateRunner a ()
tplModifyItem f =
  tplItem
    >>= f
    >>= tplReplaceItem

tplReplaceItem :: Item a -> TemplateRunner a ()
tplReplaceItem item = do
  void tplPopItem
  tplPushItem item

tplPopItem :: TemplateRunner a (Item a)
tplPopItem =
  gets tplItemStack >>= \case
    [] -> error "tplPopItem: no Item on stack"
    current : previous -> do
      modify \s -> s {tplItemStack = previous}
      return current

tplPopBody :: TemplateRunner a a
tplPopBody = itemBody <$> tplPopItem

tplPushItem :: Item a -> TemplateRunner a ()
tplPushItem item = do
  stack <- gets tplItemStack
  modify \s -> s {tplItemStack = item : stack}

tplWithItem :: Item a -> TemplateRunner a b -> TemplateRunner a b
tplWithItem item f = do
  tplPushItem item
  x <- f
  void tplPopItem
  return x

tplContext :: TemplateRunner a (Context a)
tplContext =
  gets tplContextStack >>= \case
    [] -> tplFail "tplContext: no Context on stack"
    ((_, catted) : _) -> return catted

tplPushContext :: Context a -> TemplateRunner a ()
tplPushContext context = do
  stack <-
    gets tplContextStack <&> \case
      [] -> [(context, context)]
      stack@((_, cattedParent) : _) -> (context, context <> cattedParent) : stack
  modify \s -> s {tplContextStack = stack}

tplPopContext :: TemplateRunner a (Context a)
tplPopContext =
  gets tplContextStack >>= \case
    [] -> tplFail "tplPopContext: no Context on stack"
    ((current, _) : previous) -> do
      modify \s ->
        s
          { tplContextStack = previous
          }
      return current

-- | Place context within a given scope.
tplWithContext :: Context a -> TemplateRunner a b -> TemplateRunner a b
tplWithContext context f = do
  tplPushContext context
  x <- f
  void tplPopContext
  return x

-- | Get a value from the context by name and convert it.
tplGet :: (FromValue v a) => String -> TemplateRunner a v
tplGet name =
  tplContext
    >>= flip unContext name
    >>= fromValue

-- | Get a value from a specific item's context by name and convert it.
tplGetWithItemContext :: (FromValue v a) => Item a -> Context a -> String -> TemplateRunner a v
tplGetWithItemContext item context name =
  tplWithItem item $ tplWithContext context $ tplGet name

-- | Place context in global scope.
tplPut :: Context a -> TemplateRunner a ()
tplPut context = do
  stack <- fmap (second (context <>)) <$> gets tplContextStack
  modify' \s -> s {tplContextStack = stack}

-- | Perform an action within the scope of a call.
tplWithCall :: String -> TemplateRunner a b -> TemplateRunner a b
tplWithCall call f = do
  stack <- gets tplCallStack
  modify' \s -> s {tplCallStack = call : stack}
  x <- f
  modify' \s -> s {tplCallStack = stack}
  return x

-- | Perform an action within the scope of a field call.
tplWithField :: String -> TemplateRunner a b -> TemplateRunner a b
tplWithField field' f = do
  file <- itemFilePath <$> tplItem
  tplWithCall ("field " ++ show field' ++ " in " ++ file) f

-- | Fail with an error message and trace.
tplFail :: String -> TemplateRunner a b
tplFail = fail <=< tplTraced

-- | Fail with a no-result message and trace.
tplTried :: String -> TemplateRunner a b
tplTried = lift . noResult <=< tplTraced

-- | Return the current call stack, with the most recent call first.
tplTrace :: TemplateRunner a [String]
tplTrace = gets tplCallStack

-- | Get a formatted trace message with the most recent call first.
tplTraced :: String -> TemplateRunner a String
tplTraced message = do
  trace <- tplTrace
  return $ message ++ ", trace from most recent: [" ++ intercalate ", " trace ++ "]"

-- | Apply @f@ to an item if @key@ is requested.
field :: (IntoValue v a) => String -> (Item a -> TemplateRunner a v) -> Context a
field key f = Context f'
  where
    f' k =
      tplWithField key $
        if k == key
          then do
            i <- tplItem
            intoValue <$> f i
          else do
            tplTried $ "key " ++ show k ++ " did not match field " ++ show key

-- | Reports missing field.
missingField :: Context a
missingField = Context f
  where
    f key = tplTried $ "missing " ++ show key

-- | Const-valued field, returns the same @val@ per @key@.
constField :: (IntoValue v a) => String -> v -> Context a
constField key val = field key f
  where
    constResult = return val
    f _ = tplWithCall key constResult

-- | Creates a field containing a list of items.
itemsField :: String -> Context a -> [Item a] -> Context a
itemsField key context items = constField key $ itemListValue context items

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
    h name = f name `catchError` (\_ -> g name)

-- | Lookup of @val@ by @key@ into provided @HashMap@.
hashMapField :: (IntoValue v a) => HashMap String v -> Context a
hashMapField m = Context f
  where
    m' = intoValue <$> m
    f k = tplWithCall "hashMap" $ maybe (tried k) return (HashMap.lookup k m')
    tried k = tplTried $ "tried " ++ show k ++ " from hashmap of keys " ++ show (HashMap.keys m')

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
  intoContext = ic . fmap (bimap Key.toString intoValue) . KeyMap.toList
    where
      ic :: [(String, ContextValue a)] -> Context a
      ic = intoContext

-- | ContextValues can hold certain types of data within a context.
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
    x -> tplFail $ "Tried to get " ++ show x ++ " as List"

instance FromValue' 'True String a where
  fromValue' flag = \case
    StringValue x -> return x
    EmptyValue -> return ""
    ThunkValue fx -> fromValue' flag =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as String"

instance FromValue (Context a) a where
  fromValue = \case
    ContextValue c -> return c
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Context"

instance FromValue (ContextValue a) a where
  fromValue = return

instance FromValue Bool a where
  fromValue = \case
    BoolValue x -> return x
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Bool"

instance FromValue Double a where
  fromValue = \case
    DoubleValue x -> return x
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Double"

instance FromValue Int a where
  fromValue = \case
    IntValue x -> return x
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Int"

instance FromValue (Context a, [Item a]) a where
  fromValue = \case
    ItemValue context items -> return (context, items)
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Item"

instance FromValue Block a where
  fromValue = \case
    BlockValue block -> return block
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Block"

instance (IntoValue v a, FromValue w a) => FromValue (FunctionValue v w a) a where
  fromValue cv = case cv of
    FunctionValue f -> return f'
      where
        f' v = fromValue =<< f (intoValue v)
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Function"

instance (IntoValue v a, IntoValue x a, FromValue w a) => FromValue (FunctionValue2 v x w a) a where
  fromValue cv = case cv of
    FunctionValue f -> return f'
      where
        f' v x = do
          g <- fromValue =<< f (intoValue v)
          g x
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Function2"

instance (IntoValue v a, IntoValue x a, IntoValue y a, FromValue w a) => FromValue (FunctionValue3 v x y w a) a where
  fromValue = \case
    FunctionValue f -> return f'
      where
        f' v x y = do
          g <- fromValue =<< f (intoValue v)
          h <- g x
          h y
    ThunkValue fx -> fromValue =<< fx
    x -> tplFail $ "Tried to get " ++ show x ++ " as Function3"

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
    x -> tplFail $ "Tried to get " ++ show x ++ " as Function4"
