module Green.Template.Context where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Green.Template.Ast
import Hakyll (Compiler, Item (..))
import qualified Hakyll as H
import Prelude hiding (lookup)

data Context
  = MapContext MapContext
  | FunctionContext FunctionContext

type MapContext = HashMap String ContextValue

type FunctionContext = String -> Item String -> Compiler (Maybe ContextValue)

singleton :: String -> ContextValue -> Context
singleton name value = MapContext (HashMap.singleton name value)

lookup :: String -> Context -> Item String -> Compiler (Maybe ContextValue)
lookup name (MapContext m) _ = return $ HashMap.lookup name m
lookup name (FunctionContext f) item = f name item

insert :: String -> ContextValue -> Context -> Context
insert name value = \case
  MapContext m -> MapContext (HashMap.insert name value m)
  f@FunctionContext {} -> singleton name value <> f

getContext :: H.Identifier -> Compiler Context
getContext id' = intoContext <$> H.getMetadata id'

defaultContext :: Context
defaultContext = metadataField

metadataField :: Context
metadataField = FunctionContext f
  where
    f name item = do
      context <- getContext (itemIdentifier item)
      lookup name context item

functionField :: String -> (Item String -> Compiler (Maybe ContextValue)) -> Context
functionField name f = FunctionContext f'
  where
    f' receivedName item
      | receivedName == name = f item
      | otherwise = return Nothing

instance Semigroup Context where
  f <> g = FunctionContext h
    where
      h name item =
        let lookup' = flip (lookup name) item
         in (<|>) <$> lookup' f <*> lookup' g

instance Monoid Context where
  mempty = MapContext HashMap.empty

data ContextValue
  = ContextValue Context
  | ListValue [ContextValue]
  | ErrorValue String
  | UndefinedValue String
  | BoolValue Bool
  | StringValue String
  | DoubleValue Double
  | IntValue Int
  | NameValue String
  | FunctionValue FunctionValue
  | TemplateValue Template
  | UnitValue

type FunctionValue = ContextValue -> Context -> Item String -> Compiler ContextValue

instance Show ContextValue where
  show = \case
    ContextValue {} -> "ContextValue"
    ListValue values -> "ListValue (" ++ show values ++ ")"
    ErrorValue e -> "ErrorValue " ++ show e
    UndefinedValue name -> "UndefinedValue " ++ show name
    NameValue name -> "NameValue " ++ show name
    BoolValue b -> "BoolValue " ++ show b
    StringValue t -> "StringValue " ++ show t
    DoubleValue d -> "DoubleValue " ++ show d
    IntValue i -> "IntValue " ++ show i
    FunctionValue {} -> "FunctionValue"
    TemplateValue template -> "TemplateValue (" ++ show template ++ ")"
    UnitValue -> "Unit"

class IntoContext a where
  intoContext :: a -> Context

instance IntoContext [(String, ContextValue)] where
  intoContext = intoContext . HashMap.fromList

instance IntoContext (HashMap String ContextValue) where
  intoContext = MapContext

instance IntoContext Object where
  intoContext = intoContext . HashMap.mapKeys T.unpack . HashMap.map go
    where
      go = \case
        Object o -> ContextValue $ intoContext o
        Array a -> ListValue $ Vector.toList $ Vector.map go a
        String t -> StringValue $ T.unpack t
        Number n
          | isInteger n -> IntValue $ fromJust $ toBoundedInteger n
          | otherwise -> DoubleValue $ fromRight 0.0 $ toBoundedRealFloat n
        Bool b -> BoolValue b
        Null -> UndefinedValue "null"
