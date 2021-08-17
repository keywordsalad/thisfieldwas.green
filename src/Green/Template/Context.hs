module Green.Template.Context where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Bifunctor
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Proxy
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Green.Template.Ast
import Hakyll (Compiler, Item (..))
import qualified Hakyll as H
import System.FilePath (takeBaseName)
import Text.Parsec (sourceName)
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
      | k == key = intoValue <$> f i
      | otherwise = H.noResult $ "Tried field key " ++ show key

-- | Reports missing field.
missingField :: Context a
missingField = Context f
  where
    f key _ = H.noResult $ "Missing field " ++ show key ++ " in context"

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
    h k i = fmap (intoValue . g) . fromValue =<< f k i

bindField :: (FromValue v a, IntoValue w a) => (v -> Compiler w) -> Context a -> Context a
bindField g (Context f) = Context h
  where
    h k i = fmap intoValue (g =<< fromValue =<< f k i)

bindWithItem :: (FromValue v a, IntoValue w a) => (v -> Item a -> Compiler w) -> Context a -> Context a
bindWithItem g (Context f) = Context h
  where
    h k i = fmap intoValue (flip g i =<< fromValue =<< f k i)

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
    f k _ = maybe (H.noResult $ "Tried key in map " ++ k) return (HashMap.lookup k m')

defaultContext :: Context String
defaultContext =
  mconcat
    [ bodyField "body",
      metadataField,
      urlField "url",
      pathField "path",
      titleField "title"
    ]

metadataField :: forall a. Context a
metadataField = Context f
  where
    f :: ContextFunction a
    f key item = do
      m <- H.getMetadata (itemIdentifier item)
      maybe
        (fail $ "Key " ++ show key ++ " not found in metadata")
        (return . intoValue)
        (HashMap.lookup (T.pack key) m)

bodyField :: (IntoValue a a) => String -> Context a
bodyField key = field key (return . itemBody)

urlField :: String -> Context a
urlField key = field key f
  where
    f item =
      let id' = itemIdentifier item
          empty' = fail $ "No route url found for item " ++ show id'
       in maybe empty' H.toUrl <$> H.getRoute id'

pathField :: String -> Context a
pathField key = field key $ return . H.toFilePath . itemIdentifier

titleField :: String -> Context a
titleField = bindField titleFromPath . pathField
  where
    titleFromPath = return . takeBaseName

teaserField :: String -> H.Snapshot -> Context String
teaserField key snapshot = field key f
  where
    f item =
      takeTeaser "" <$> H.loadSnapshotBody (itemIdentifier item) snapshot

    teaserComment = "<!-- teaser -->"
    takeTeaser acc body@(x : rest)
      | body `startsWith` teaserComment = reverse acc
      | otherwise = takeTeaser (x : acc) rest
    takeTeaser _ [] = ""

    startsWith (x : xs) (y : ys)
      | x == y = startsWith xs ys
      | otherwise = False
    startsWith _ [] = True
    startsWith [] _ = False

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
  = ContextValue (Context a)
  | ListValue [ContextValue a]
  | BoolValue Bool
  | StringValue String
  | DoubleValue Double
  | IntValue Int
  | NameValue String
  | FunctionValue (ContextValue a -> Context a -> Item a -> Compiler (ContextValue a))
  | TemplateValue Template
  | EmptyValue

getValueType :: ContextValue a -> String
getValueType = \case
  ContextValue {} -> "Context"
  ListValue {} -> "List"
  BoolValue {} -> "Bool"
  StringValue {} -> "String"
  DoubleValue {} -> "Double"
  IntValue {} -> "Int"
  NameValue name -> "Name " ++ show name
  FunctionValue {} -> "Function"
  TemplateValue (Template _ pos) -> "Template " ++ show (sourceName pos)
  EmptyValue -> "Empty"

mapString :: (IntoValue v a) => (String -> v) -> ContextValue a -> Compiler (ContextValue a)
mapString f = \case
  StringValue s -> return $ intoValue $ f s
  x -> fail $ "Could not map " ++ getValueType x ++ " as String"

flatMapString :: (IntoValue v a) => (String -> Compiler v) -> ContextValue a -> Compiler (ContextValue a)
flatMapString f = \case
  StringValue s -> intoValue <$> f s
  x -> fail $ "Could not bind " ++ getValueType x ++ " as String"

intoString :: ContextValue a -> Compiler String
intoString = \case
  StringValue s -> return s
  x -> fail $ "Could not get string from " ++ getValueType x

type FunctionValue v w a = v -> Context a -> Item a -> Compiler w

type FunctionValue2 v x w a = v -> FunctionValue x w a

type FunctionValue3 v x y w a = v -> FunctionValue2 x y w a

type FunctionValue4 v x y z w a = v -> FunctionValue3 x y z w a

instance Show (ContextValue a) where
  show = \case
    ContextValue {} -> "ContextValue"
    ListValue values -> "ListValue (" ++ show values ++ ")"
    NameValue name -> "NameValue " ++ show name
    BoolValue b -> "BoolValue " ++ show b
    StringValue t -> "StringValue " ++ show t
    DoubleValue d -> "DoubleValue " ++ show d
    IntValue i -> "IntValue " ++ show i
    FunctionValue {} -> "FunctionValue"
    TemplateValue template -> "TemplateValue (" ++ show template ++ ")"
    EmptyValue -> "Empty"

-- "Specialize" List
type family FString a :: Bool where
  FString Char = 'True
  FString _ = 'False

class IntoValue' (flag :: Bool) v a where
  intoValue' :: Proxy flag -> v -> ContextValue a

class IntoValue v a where
  intoValue :: v -> ContextValue a

instance (FString v ~ flag, IntoValue' flag [v] a) => IntoValue [v] a where
  intoValue = intoValue' (Proxy :: Proxy flag)

instance (IntoValue v a) => IntoValue' 'False [v] a where
  intoValue' _ = ListValue . (intoValue <$>)

instance IntoValue' 'True String a where
  intoValue' _ = StringValue

instance IntoValue (ContextValue a) a where
  intoValue = id

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

instance IntoValue Template a where
  intoValue = TemplateValue

instance (FromValue v a, IntoValue w a) => IntoValue (FunctionValue v w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv context item = do
        v <- fromValue cv
        intoValue <$> f v context item

instance (FromValue v a, FromValue x a, IntoValue w a) => IntoValue (FunctionValue2 v x w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv _ _ = intoValue . f <$> fromValue cv

instance (FromValue v a, FromValue x a, FromValue y a, IntoValue w a) => IntoValue (FunctionValue3 v x y w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv _ _ = intoValue . f <$> fromValue cv

instance (FromValue v a, FromValue x a, FromValue y a, FromValue z a, IntoValue w a) => IntoValue (FunctionValue4 v x y z w a) a where
  intoValue f = FunctionValue f'
    where
      f' cv _ _ = intoValue . f <$> fromValue cv

class FromValue v a where
  fromValue :: ContextValue a -> Compiler v

class FromValue' (flag :: Bool) v a where
  fromValue' :: Proxy flag -> ContextValue a -> Compiler v

instance (FString v ~ flag, FromValue' flag [v] a) => FromValue [v] a where
  fromValue = fromValue' (Proxy :: Proxy flag)

instance (FromValue v a) => FromValue' 'False [v] a where
  fromValue' _ = \case
    ListValue xs -> sequence $ fromValue <$> xs
    x -> fail $ "Tried to get " ++ getValueType x ++ " as List"

instance FromValue' 'True String a where
  fromValue' _ = \case
    StringValue x -> return x
    x -> fail $ "Tried to get " ++ getValueType x ++ " as String"

instance FromValue (Context a) a where
  fromValue = \case
    ContextValue c -> return c
    x -> fail $ "Tried to get " ++ getValueType x ++ " as Context"

instance FromValue Bool a where
  fromValue = \case
    BoolValue x -> return x
    x -> fail $ "Tried to get " ++ getValueType x ++ " as Bool"

instance FromValue Double a where
  fromValue = \case
    DoubleValue x -> return x
    x -> fail $ "Tried to get " ++ getValueType x ++ " as Double"

instance FromValue Int a where
  fromValue = \case
    IntValue x -> return x
    x -> fail $ "Tried to get " ++ getValueType x ++ " as Int"

instance FromValue Template a where
  fromValue = \case
    TemplateValue f -> return f
    x -> fail $ "Tried to get " ++ getValueType x ++ " as Template"
