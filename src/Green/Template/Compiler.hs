module Green.Template.Compiler
  ( getContext,
    applyAsTemplate,
    templateCompiler,
    templateBodyCompiler,
  )
where

import Data.List (intercalate)
import Green.Template.Context
import Green.Template.Data
import Green.Template.Parser (parseTemplate)
import Hakyll
  ( Identifier,
    Item (..),
    MonadMetadata (..),
    fromFilePath,
    getResourceBody,
    getUnderlying,
    loadBody,
    makeItem,
  )
import Hakyll.Core.Compiler.Internal
import Prelude hiding (exp, filter, lookup)

getContext :: Identifier -> Compiler Context
getContext = fmap fieldsFromMetadata . getMetadata

applyAsTemplate :: Context -> Compiler (Item String)
applyAsTemplate context = do
  context' <- (context <>) <$> (getContext =<< getUnderlying)
  templateItem <- templateCompiler
  stringItem <- getResourceBody
  makeItem =<< applyTemplate context' templateItem stringItem

templateCompiler :: Compiler (Item Template)
templateCompiler = getResourceBody >>= templateBodyCompiler . itemBody

templateBodyCompiler :: String -> Compiler (Item Template)
templateBodyCompiler = either (compilerThrow . pure) makeItem . parseTemplate

applyTemplate :: Context -> Item Template -> Item String -> Compiler String
applyTemplate context (Item _ (Template blocks)) item =
  mconcat <$> sequence (go <$> blocks)
  where
    go t = case t of
      TextBlock text' -> return text'
      CommentBlock _ -> return ""
      TemplateBlock exp truthy falsy -> do
        exp' <- evaluate context item exp
        render <$> evaluateTemplate context item exp' truthy falsy
      ExpressionBlock exp -> do
        render <$> evaluate context item exp

render :: ContextValue -> String
render = \case
  StringValue value -> value
  BoolValue bool -> show bool
  DoubleValue double -> show double
  IntValue int -> show int
  ContextValue {} -> "{ context }"
  FunctionValue {} -> "{ function }"
  TemplateValue {} -> "{ template }"
  ListValue values -> "{ " ++ intercalate ", " (render <$> values) ++ " }"
  ErrorValue message -> "{ error " ++ show message ++ " }"
  UndefinedValue name -> "{ undefined " ++ show name ++ " }"
  NameValue name -> "{ name " ++ show name ++ " }"

evaluate :: Context -> Item String -> Expression -> Compiler ContextValue
evaluate context item expression =
  case expression of
    ContextExpression keyExps -> evaluateContext keyExps
    NameExpression name -> return $ NameValue name
    StringExpression text -> return $ StringValue text
    IntExpression int -> return $ IntValue int
    DoubleExpression double -> return $ DoubleValue double
    BoolExpression bool -> return $ BoolValue bool
    ApplyExpression start rest -> runList apply start rest
    AccessExpression start rest -> runList access start rest
  where
    go = evaluate context item

    evaluateContext keyExps =
      ContextValue . fieldsFromList
        <$> mapM sequence (fmap (evaluate context item) <$> keyExps)

    runList f start rest = do
      start' <- go start
      rest' <- sequence $ go <$> rest
      runList' f start' rest'

    runList' f start (x : xs) = f context item start x >>= flip (runList' f) xs
    runList' _ done [] = return done

apply :: Context -> Item String -> ContextValue -> ContextValue -> Compiler ContextValue
apply context item (FunctionValue f) argument = f context argument item
apply context item (NameValue name) argument = do
  result <- lookup context name item
  apply context item (lookupToValue result) argument
apply _ _ fn argument =
  return $ ErrorValue $ "Tried to apply non-function " ++ show fn ++ " to argument " ++ show argument

access :: Context -> Item String -> ContextValue -> ContextValue -> Compiler ContextValue
access context item c@(ContextValue context') field' =
  case field' of
    NameValue name -> lookupToValue <$> lookup context' name item
    StringValue name -> lookupToValue <$> lookup context' name item
    f@FunctionValue {} -> apply context item f c
    _ -> return $ ErrorValue $ "Tried to access field from context using " ++ show field'
access context item target f@FunctionValue {} = apply context item f target
access _ _ target field' =
  return $ ErrorValue $ "Tried to access field from " ++ show target ++ " using " ++ show field'

evaluateTemplate :: Context -> Item String -> ContextValue -> Template -> Template -> Compiler ContextValue
evaluateTemplate context item exp truthy falsy = undefined
