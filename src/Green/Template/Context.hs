module Green.Template.Context where

import Control.Applicative ((<|>))
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
import Green.Template.Ast
import Hakyll (Compiler, Item (..))
import qualified Hakyll as H
import Prelude hiding (lookup)

newtype Context a = Context {unContext :: ContextFunction a}

type ContextFunction a = String -> Item a -> Compiler (ContextValue a)

getContext :: H.Identifier -> Compiler (Context a)
getContext id' = intoContext <$> H.getMetadata id'

-- | Apply @f@ to an item if @key@ is requested.
field :: (IntoValue v a) => String -> (Item a -> Compiler v) -> Context a
field key f = Context f'
  where
    f' k i
      | k == key = tryWithError k i (intoValue <$> f i)
      | otherwise = H.noResult $ "Tried field key " ++ show key

tryWithError :: String -> Item a -> Compiler b -> Compiler b
tryWithError key item =
  H.withErrorMessage $
    "Error getting field " ++ show key
      ++ " for item "
      ++ show (H.itemIdentifier item)

-- | Reports missing field.
missingField :: Context a
missingField = Context f
  where
    f key item =
      tryWithError key item $
        H.noResult $ "Missing field " ++ show key ++ " in context"

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
    h k i = tryWithError k i $ fmap (intoValue . g) . fromValue =<< f k i

bindField :: (FromValue v a, IntoValue w a) => (v -> Compiler w) -> Context a -> Context a
bindField g (Context f) = Context h
  where
    h k i = tryWithError k i $ fmap intoValue (g =<< fromValue =<< f k i)

-- | Alternation of context @g@ after context @f@.
composeField :: Context a -> Context a -> Context a
composeField (Context g) (Context f) = Context h
  where
    h name item = f name item <|> g name item

-- | Lookup of @val@ by @key@ into provided @HashMap@.
hashMapField :: (IntoValue v a) => HashMap String v -> Context a
hashMapField m = Context f
  where
    m' = intoValue <$> m
    f k _ = maybe (tried k) return (HashMap.lookup k m')
    tried k = H.noResult $ "Tried field in map " ++ k

functionField :: (FromValue v a, IntoValue w a) => String -> (v -> Context a -> Item a -> Compiler w) -> Context a
functionField = constField

functionField2 :: (FromValue v a, FromValue x a, IntoValue w a) => String -> (v -> x -> Context a -> Item a -> Compiler w) -> Context a
functionField2 = constField

functionField3 :: (FromValue v a, FromValue x a, FromValue y a, IntoValue w a) => String -> (v -> x -> y -> Context a -> Item a -> Compiler w) -> Context a
functionField3 = constField

functionField4 :: (FromValue v a, FromValue x a, FromValue y a, FromValue z a, IntoValue w a) => String -> (v -> x -> y -> z -> Context a -> Item a -> Compiler w) -> Context a
functionField4 = constField

instance Semigroup (Context a) where
  (<>) = flip composeField

instance Monoid (Context a) where
  mempty = missingField

class IntoContext v a where
  intoContext :: v -> Context a

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
  | UndefinedValue String
  | ContextValue (Context a)
  | ListValue [ContextValue a]
  | BoolValue Bool
  | StringValue String
  | DoubleValue Double
  | IntValue Int
  | FunctionValue (ContextValue a -> Context a -> Item a -> Compiler (ContextValue a))
  | BlockValue Block
  | ItemValue (Item a)
  | ThunkValue (Compiler (ContextValue a))

type FunctionValue v w a = v -> Context a -> Item a -> Compiler w

type FunctionValue2 v x w a = v -> FunctionValue x w a

type FunctionValue3 v x y w a = v -> FunctionValue2 x y w a

type FunctionValue4 v x y z w a = v -> FunctionValue3 x y z w a

instance Show (ContextValue a) where
  show = \case
    EmptyValue -> "EmptyValue"
    UndefinedValue name -> "UndefinedValue " ++ show name
    ContextValue {} -> "ContextValue"
    ListValue values -> "ListValue (" ++ show values ++ ")"
    BoolValue b -> "BoolValue " ++ show b
    StringValue t -> "StringValue " ++ show t
    DoubleValue d -> "DoubleValue " ++ show d
    IntValue i -> "IntValue " ++ show i
    FunctionValue {} -> "FunctionValue"
    BlockValue {} -> "BlockValue"
    ItemValue item -> "Item " ++ show (itemIdentifier item)
    ThunkValue {} -> "ThunkValue"

class IntoValue' (flag :: Bool) v a where
  intoValue' :: Proxy flag -> v -> ContextValue a

-- "Specialize" List
type family FString a :: Bool where
  FString Char = 'True
  FString _ = 'False

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

instance IntoValue Bool a where
  intoValue = BoolValue

instance IntoValue Double a where
  intoValue = DoubleValue

instance IntoValue Int a where
  intoValue = IntValue

instance IntoValue (Item a) a where
  intoValue = ItemValue

instance (IntoValue v a) => IntoValue (Maybe v) a where
  intoValue (Just v) = intoValue v
  intoValue Nothing = EmptyValue

instance (FromValue v a, IntoValue w a) => IntoValue (FunctionValue v w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv context item = do
        v <- tryWithError "into function1" item $ fromValue cv
        intoValue <$> f v context item

instance (FromValue v a, FromValue x a, IntoValue w a) => IntoValue (FunctionValue2 v x w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv _ item =
        tryWithError "into function2" item $
          intoValue . f <$> fromValue cv

instance (FromValue v a, FromValue x a, FromValue y a, IntoValue w a) => IntoValue (FunctionValue3 v x y w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv _ item =
        tryWithError "into function3" item $
          intoValue . f <$> fromValue cv

instance (FromValue v a, FromValue x a, FromValue y a, FromValue z a, IntoValue w a) => IntoValue (FunctionValue4 v x y z w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv _ item =
        tryWithError "into function4" item $
          intoValue . f <$> fromValue cv

instance IntoValue (Compiler (ContextValue a)) a where
  intoValue = ThunkValue

class FromValue v a where
  fromValue :: ContextValue a -> Compiler v

class FromValue' (flag :: Bool) v a where
  fromValue' :: Proxy flag -> ContextValue a -> Compiler v

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

instance FromValue (Item a) a where
  fromValue = \case
    ItemValue item -> return item
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Item"

instance FromValue Block a where
  fromValue = \case
    BlockValue block -> return block
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Block"

instance (IntoValue v a, FromValue w a) => FromValue (FunctionValue v w a) a where
  fromValue = \case
    FunctionValue f -> return f'
      where
        f' v context item = do
          tryWithError "from function1" item $
            fromValue =<< f (intoValue v) context item
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Function"

instance (IntoValue v a, IntoValue x a, FromValue w a) => FromValue (FunctionValue2 v x w a) a where
  fromValue = \case
    FunctionValue f -> return f'
      where
        f' v x context item = do
          g <-
            tryWithError "from function2" item $
              fromValue =<< f (intoValue v) context item
          g x context item
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Function"

instance (IntoValue v a, IntoValue x a, IntoValue y a, FromValue w a) => FromValue (FunctionValue3 v x y w a) a where
  fromValue = \case
    FunctionValue f -> return f'
      where
        f' v x y context item = do
          g <-
            tryWithError "from function3" item $
              fromValue =<< f (intoValue v) context item
          h <- g x context item
          h y context item
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Function"

instance (IntoValue v a, IntoValue x a, IntoValue y a, IntoValue z a, FromValue w a) => FromValue (FunctionValue4 v x y z w a) a where
  fromValue = \case
    FunctionValue f -> return f'
      where
        f' v x y z context item = do
          g <-
            tryWithError "from function3" item $
              fromValue =<< f (intoValue v) context item
          h <- g x context item
          i <- h y context item
          i z context item
    ThunkValue fx -> fromValue =<< fx
    x -> fail $ "Tried to get " ++ show x ++ " as Function"
