module Green.Template.Compiler where

import Control.Monad.Except
import Green.Template.Context
import Green.Template.Data
import Green.Template.Parser (parseTemplate)
import Hakyll
  ( Compiler,
    Identifier,
    Item (..),
    MonadMetadata (..),
    cached,
    debugCompiler,
    getResourceBody,
    getUnderlying,
    makeItem,
    toFilePath,
    withItemBody,
  )
import Prelude hiding (exp, filter, lookup)

templateCompiler :: Compiler (Item Template)
templateCompiler = cached "Green.Template.Compiler.templateCompiler" do
  id' <- getUnderlying
  item <- getResourceBody
  debugCompiler $ "Compiling template from " ++ show id'
  withItemBody (compileTemplateFile id') item

applyTemplate :: Template -> Context -> Item String -> Compiler (Item String)
applyTemplate (Template origin blocks) _context item = do
  debugCompiler $ "Applying template " ++ show origin ++ " to " ++ show (itemIdentifier item)
  sequence (goBlock <$> blocks)
    >>= fmap mconcat . mapM render
    >>= makeItem
  where
    goBlock block =
      case block of
        CommentBlock _ -> return $ StringValue ""
        TextBlock value -> return $ StringValue value
        TemplateBlock {} -> undefined
        ExpressionBlock {} -> undefined

applyAsTemplate :: Context -> Item String -> Compiler (Item String)
applyAsTemplate context item = do
  debugCompiler $ "Applying " ++ show (itemIdentifier item) ++ " as template"
  template <- compileTemplateFile (itemIdentifier item) (itemBody item)
  applyTemplate template context item

compileTemplateFile :: Identifier -> String -> Compiler Template
compileTemplateFile id' input = do
  debugCompiler $ "Compiling template file " ++ show id'
  case parseTemplate (toFilePath id') input of
    Right template -> do
      debugCompiler $ "Parsed template from file " ++ show id'
      return template
    Left e -> do
      debugCompiler $ "Failed to parse template file " ++ show id' ++ ": " ++ show e
      fail e

render :: (MonadFail m) => ContextValue -> m String
render = \case
  StringValue value -> return value
  BoolValue value -> return $ show value
  DoubleValue value -> return $ show value
  IntValue value -> return $ show value
  ListValue values -> mconcat <$> sequence (render <$> values)
  ErrorValue message -> fail message
  UndefinedValue name -> fail $ "Tried to render undefined value identified by " ++ show name
  TemplateValue template -> fail $ "Tried to render template value " ++ show (templateOrigin template)
  NameValue name -> fail $ "Tried to render unresolved name value " ++ show name
  ContextValue {} -> fail "Tried to render context value"
  FunctionValue {} -> fail "Tried to render function value"

getContext :: Identifier -> Compiler Context
getContext = fmap contextFromMetadata . getMetadata

defaultContext :: Context
defaultContext = undefined

contextFn :: (ContextValue -> Context -> Item String -> Compiler ContextValue) -> ContextValue
contextFn = FunctionValue

contextFn2 :: (ContextValue -> ContextValue -> Context -> Item String -> Compiler ContextValue) -> ContextValue
contextFn2 f2 = FunctionValue f1
  where
    f1 arg1 _ _ = return $ FunctionValue (f2 arg1)

contextFn3 :: (ContextValue -> ContextValue -> ContextValue -> Context -> Item String -> Compiler ContextValue) -> ContextValue
contextFn3 f3 = FunctionValue f1
  where
    f1 arg1 _ _ = return $ FunctionValue (f2 arg1)
    f2 arg1 arg2 _ _ = return $ FunctionValue (f3 arg1 arg2)
