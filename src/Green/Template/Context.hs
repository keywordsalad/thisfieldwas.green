module Green.Template.Context where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Either
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Vector as Vector
import Green.Template.Data
import Hakyll hiding (Context, Template)
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
  | FunctionValue (Context -> ContextValue -> Item String -> Compiler ContextValue)
  | TemplateValue Template

instance Show ContextValue where
  show v = case v of
    ContextValue _ -> "(ContextValue)"
    ListValue values -> "(ListValue " ++ show values ++ ")"
    ErrorValue e -> "(ErrorValue " ++ show e ++ ")"
    UndefinedValue name -> "(UndefinedValue " ++ show name ++ ")"
    NameValue name -> "(NameValue " ++ show name ++ ")"
    BoolValue b -> "(BoolValue " ++ show b ++ ")"
    StringValue t -> "(StringValue " ++ show t ++ ")"
    DoubleValue d -> "(DoubleValue " ++ show d ++ ")"
    IntValue i -> "(IntValue " ++ show i ++ ")"
    FunctionValue _ -> "(FunctionValue)"
    TemplateValue _ -> "(TemplateValue)"

isTruthy :: ContextValue -> Bool
isTruthy v = case v of
  ContextValue _ -> True
  NameValue _ -> True
  FunctionValue _ -> True
  BoolValue bool -> bool
  DoubleValue double -> double > 0.0
  IntValue int -> int > 0
  StringValue value -> not (null value)
  ListValue values -> not (null values)
  TemplateValue (Template blocks) -> not (null blocks)
  _ -> False

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

newtype Context = Context {lookup :: String -> Item String -> Compiler LookupResult}

instance Semigroup Context where
  Context f <> Context g = Context h
    where
      h name item = f name item <|> g name item

instance Monoid Context where
  mempty = emptyField

emptyField :: Context
emptyField = Context f
  where
    f name _ = return $ LookupMissing name

functionField :: String -> (String -> Item String -> Compiler LookupResult) -> Context
functionField name f = Context f'
  where
    f' name' item =
      if name == name'
        then f name' item
        else return $ LookupMissing name'

field :: String -> ContextValue -> Context
field name val = Context f
  where
    f name' _ =
      return
        if name' == name
          then LookupFound val
          else LookupMissing name'

fieldsFromList :: [(String, ContextValue)] -> Context
fieldsFromList keyVals = fieldsFromMap (HashMap.fromList keyVals)

fieldsFromMap :: HashMap.HashMap String ContextValue -> Context
fieldsFromMap contextMap = Context f
  where
    f name _ =
      return $
        maybe (LookupMissing name) LookupFound (HashMap.lookup name contextMap)

fieldsFromMetadata :: Metadata -> Context
fieldsFromMetadata = fromObject
{-# INLINE fieldsFromMetadata #-}

fromObject :: Object -> Context
fromObject o = fieldsFromMap $ HashMap.mapKeys T.unpack (HashMap.map fromAeson o)

fromAeson :: Value -> ContextValue
fromAeson v = case v of
  Object o -> ContextValue $ fromObject o
  Array a -> ListValue $ Vector.toList $ Vector.map fromAeson a
  String t -> StringValue $ T.unpack t
  Number n
    | Scientific.isInteger n -> IntValue $ fromJust $ Scientific.toBoundedInteger n
    | otherwise -> DoubleValue $ fromRight 0.0 $ Scientific.toBoundedRealFloat n
  Bool b -> BoolValue b
  Null -> UndefinedValue "null"
