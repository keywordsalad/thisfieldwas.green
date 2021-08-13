module Green.Template.Compiler where

import Data.Foldable (foldlM)
import Data.List (intercalate)
import Data.Maybe
import Data.Traversable
import Green.Template.Ast
import Green.Template.Context
import Green.Template.Parser (parseTemplate)
import Hakyll hiding (Context, Template, applyTemplate, compileTemplateItem, field)
import Text.Parsec (SourcePos, sourceName)
import Prelude hiding (lookup)

isTruthy :: (MonadFail m) => ContextValue -> m Bool
isTruthy = \case
  ContextValue _ -> return True
  ListValue values -> return $ not (null values)
  ErrorValue {} -> return False
  UndefinedValue {} -> return False
  BoolValue value -> return value
  StringValue value -> return $ not (null value)
  DoubleValue value -> return $ value /= 0
  IntValue value -> return $ value /= 0
  NameValue name -> fail $ "Unevaluated name " ++ show name
  FunctionValue {} -> return True
  TemplateValue (Template blocks _) -> return $ not (null blocks)
  UnitValue {} -> return False

isFalsy :: (MonadFail m) => ContextValue -> m Bool
isFalsy = fmap not . isTruthy

templateCompiler :: Compiler (Item Template)
templateCompiler =
  cached "Green.Template.Compiler.templateCompiler" $
    getResourceBody
      >>= compileTemplateItem
      >>= makeItem

compileTemplateItem :: Item String -> Compiler Template
compileTemplateItem item = do
  filePath <- getResourceFilePath
  debugCompiler $ "Compiling template from " ++ show filePath
  either (fail . show) return (parseTemplate filePath (itemBody item))

applyAsTemplate :: Context -> Item String -> Compiler (Item String)
applyAsTemplate context item = do
  template <- compileTemplateItem item
  applyTemplate template context item

applyTemplate :: Template -> Context -> Item String -> Compiler (Item String)
applyTemplate template context item = do
  let (Template blocks pos) = template
  let id' = itemIdentifier item
  debugCompiler $ "Applying template " ++ show (sourceName pos) ++ " to item " ++ show id'
  context' <- (context <>) <$> getContext (itemIdentifier item)
  makeItem =<< applyBlocks context' blocks item

applyBlocks :: Context -> [Block] -> Item String -> Compiler String
applyBlocks context (block : rest) item = do
  thisResult <- applyBlock context block item
  restResults <- applyBlocks context rest item
  return (thisResult ++ restResults)
applyBlocks _ [] _ = return ""

applyBlock :: Context -> Block -> Item String -> Compiler String
applyBlock context block item = case block of
  TextBlock value _ -> return value
  ExpressionBlock expression _ -> stringify <$> eval context expression item
  CommentBlock _ _ -> return ""
  LayoutApplyBlock expression template _ -> snd <$> applyGuard expression template
  TemplateBlock blocks defaultBlocks _ ->
    foldlM applyTemplateBlocks (False, "") blocks >>= \case
      (True, result) -> return result
      _ -> applyDefaultBlocks
    where
      applyTemplateBlocks result@(stop, _) (TemplateApplyBlock expression template _)
        | stop = return result
        | otherwise = applyGuard expression template
      applyDefaultBlocks =
        case defaultBlocks of
          Just (TemplateDefaultBlock (Template blocks' _) _) -> applyBlocks context blocks' item
          Nothing -> return ""
  _ -> fail $ "Unexpected block in " ++ show (getBlockPos block)
  where
    stringify = \case
      StringValue value -> value
      IntValue value -> show value
      DoubleValue value -> show value
      BoolValue value -> show value
      ListValue values -> intercalate "" (stringify <$> values)
      x -> show x
    applyGuard guardExp template@(Template blocks _) =
      eval context guardExp item >>= \case
        FunctionValue f -> do
          result <- f (TemplateValue template) context item
          truthy <- isTruthy result
          return (truthy, stringify result)
        guard@(ContextValue context') ->
          isTruthy guard >>= \case
            True -> (True,) <$> applyBlocks (context' <> context) blocks item
            False -> return (False, "")
        guard ->
          isTruthy guard >>= \case
            True -> (True,) <$> applyBlocks context blocks item
            False -> return (False, "")

eval :: Context -> Expression -> Item String -> Compiler ContextValue
eval context expression item = case expression of
  StringExpression value _ -> return $ StringValue value
  IntExpression value _ -> return $ IntValue value
  DoubleExpression value _ -> return $ DoubleValue value
  BoolExpression value _ -> return $ BoolValue value
  --
  ApplyExpression f x pos -> apply f x pos context item
  FilterExpression x f pos -> apply f x pos context item
  --
  ListExpression values _ -> do
    let eval' e = eval context e item
    ListValue <$> mapM eval' values
  --
  ContextExpression expKeyVals _ -> do
    let evalKV k v = (k,) <$> eval context v item
    keyVals <- mapM (uncurry evalKV) expKeyVals
    return $ ContextValue (intoContext keyVals)
  --
  NameExpression name pos -> do
    maybeVal <- lookup name context item
    maybe (undefinedValue name pos) return maybeVal
  --
  AccessExpression targetExp fieldExp pos ->
    eval context targetExp item >>= \case
      ContextValue target ->
        case fieldExp of
          NameExpression field _ -> do
            result <- lookup field target item
            maybe (undefinedValue field pos) return result
          _ ->
            eval context fieldExp item >>= \case
              StringValue field -> do
                result <- lookup field target item
                maybe (undefinedValue field pos) return result
              _ -> fail $ "Can't access field from context in " ++ show pos
      ListValue list ->
        eval context fieldExp item >>= \case
          IntValue index
            | index < length list -> return $ list !! index
            | otherwise -> fail $ "Index " ++ show index ++ " out of bounds in " ++ show pos
          val -> fail $ "Can't index into list with " ++ show val ++ " in " ++ show pos
      _ -> fail $ "Can't access field from expression in " ++ show pos

apply :: Expression -> Expression -> SourcePos -> Context -> Item String -> Compiler ContextValue
apply fExp xExp pos context item =
  eval context fExp item >>= \case
    FunctionValue f' -> do
      x <- eval context xExp item
      f' x context item
    x -> fail $ "Can't apply " ++ show x ++ " as function in " ++ show pos

asString :: Expression -> Compiler String
asString = \case
  StringExpression value _ -> return value
  IntExpression value _ -> return $ show value
  DoubleExpression value _ -> return $ show value
  BoolExpression value _ -> return $ show value
  e -> fail $ "Unevaluated " ++ show e ++ " in " ++ show (getExpressionPos e)

undefinedValue :: String -> SourcePos -> Compiler a
undefinedValue name pos = fail $ "Undefined value " ++ show name ++ " in " ++ show pos
