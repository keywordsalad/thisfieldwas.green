module Green.Template.Context where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Either
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Green.Template.Ast
import Hakyll (Compiler, Item (..), Metadata)
import Prelude hiding (lookup)

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
  | FunctionValue (ContextValue -> Context -> Item String -> Compiler ContextValue)
  | TemplateValue Template

trueValue :: ContextValue
trueValue = BoolValue True

falseValue :: ContextValue
falseValue = BoolValue False

instance Show ContextValue where
  show = \case
    ContextValue {} -> "(ContextValue)"
    ListValue values -> "(ListValue " ++ show values ++ ")"
    ErrorValue e -> "(ErrorValue " ++ show e ++ ")"
    UndefinedValue name -> "(UndefinedValue " ++ show name ++ ")"
    NameValue name -> "(NameValue " ++ show name ++ ")"
    BoolValue b -> "(BoolValue " ++ show b ++ ")"
    StringValue t -> "(StringValue " ++ show t ++ ")"
    DoubleValue d -> "(DoubleValue " ++ show d ++ ")"
    IntValue i -> "(IntValue " ++ show i ++ ")"
    FunctionValue {} -> "(FunctionValue)"
    TemplateValue template -> "(TemplateValue " ++ show template ++ ")"

isTruthy :: ContextValue -> Bool
isTruthy = \case
  ContextValue _ -> True
  NameValue _ -> True
  FunctionValue _ -> True
  BoolValue bool -> bool
  DoubleValue double -> double > 0.0
  IntValue int -> int > 0
  StringValue value -> not (null value)
  ListValue values -> not (null values)
  TemplateValue (Template blocks _) -> not (null blocks)
  _ -> False

isFalsy :: ContextValue -> Bool
isFalsy = not . isTruthy

data LookupResult
  = LookupFound ContextValue
  | LookupMissing String
  deriving stock (Show)

lookupToValue :: LookupResult -> ContextValue
lookupToValue = \case
  LookupFound value -> value
  LookupMissing name -> UndefinedValue name

lookupFound :: (Monad m) => ContextValue -> m LookupResult
lookupFound = return . LookupFound

lookupMissing :: (Monad m) => String -> m LookupResult
lookupMissing = return . LookupMissing

data Context
  = MapContext (HashMap String ContextValue)
  | FunctionContext (String -> Item String -> Compiler LookupResult)

instance Semigroup Context where
  f <> g = FunctionContext h
    where
      h name item = (verify =<< lookup f name item) <|> lookup g name item
      verify = \case
        x@LookupFound {} -> return x
        LookupMissing name -> error $ "Did not find " ++ name ++ " in context"

instance Monoid Context where
  mempty = FunctionContext (const . lookupMissing)

lookup :: Context -> String -> Item String -> Compiler LookupResult
lookup (MapContext m) name _ = maybe (lookupMissing name) lookupFound (HashMap.lookup name m)
lookup (FunctionContext f) name item = f name item

contextFromList :: [(String, ContextValue)] -> Context
contextFromList keyVals = contextFromMap (HashMap.fromList keyVals)

contextFromMap :: HashMap String ContextValue -> Context
contextFromMap = MapContext

contextFromMetadata :: Metadata -> Context
contextFromMetadata = contextFromObject
{-# INLINE contextFromMetadata #-}

contextFromObject :: Object -> Context
contextFromObject o = contextFromMap $ HashMap.mapKeys T.unpack (HashMap.map go o)
  where
    go = \case
      Object o' -> ContextValue $ contextFromObject o'
      Array a -> ListValue $ Vector.toList $ Vector.map go a
      String t -> StringValue $ T.unpack t
      Number n
        | Scientific.isInteger n -> IntValue $ fromJust $ Scientific.toBoundedInteger n
        | otherwise -> DoubleValue $ fromRight 0.0 $ Scientific.toBoundedRealFloat n
      Bool b -> BoolValue b
      Null -> UndefinedValue "null"
