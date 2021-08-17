module Green.Template.Compiler
  ( applyAsTemplate,
    applyTemplate,
    compileTemplateItem,
    loadAndApplyTemplate,
    templateCompiler,
  )
where

import Data.Foldable (foldlM)
import Data.List (intercalate)
import Data.Maybe
import Green.Template.Ast
import Green.Template.Context
import Green.Template.Parser (parseTemplate)
import Hakyll (Compiler, Item)
import qualified Hakyll as H
import Text.Parsec (SourcePos, sourceName)
import Prelude hiding (lookup)

isTruthy :: (MonadFail m) => ContextValue a -> m Bool
isTruthy = \case
  ContextValue {} -> return True
  ListValue values -> return $ not (null values)
  BoolValue value -> return value
  StringValue value -> return $ not (null value)
  DoubleValue value -> return $ value /= 0
  IntValue value -> return $ value /= 0
  NameValue name -> fail $ "Unevaluated name " ++ show name
  FunctionValue {} -> return True
  TemplateValue (Template blocks _) -> return $ not (null blocks)
  EmptyValue {} -> return False

templateCompiler :: Compiler (Item Template)
templateCompiler =
  H.cached "Green.Template.Compiler.templateCompiler" $
    H.getResourceBody
      >>= compileTemplateItem
      >>= H.makeItem

compileTemplateItem :: Item String -> Compiler Template
compileTemplateItem item = do
  filePath <- H.getResourceFilePath
  H.debugCompiler $ "Compiling template from " ++ show filePath
  either (fail . show) return (parseTemplate filePath (H.itemBody item))

applyAsTemplate :: Context String -> Item String -> Compiler (Item String)
applyAsTemplate context item = do
  template <- compileTemplateItem item
  applyTemplate template context item

applyTemplate :: Template -> Context a -> Item a -> Compiler (Item String)
applyTemplate template context item = do
  let (Template blocks pos) = template
  let id' = H.itemIdentifier item
  H.debugCompiler $ "Applying template " ++ show (sourceName pos) ++ " to item " ++ show id'
  context' <- (context <>) <$> getContext (H.itemIdentifier item)
  H.makeItem =<< applyBlocks context' blocks item

loadAndApplyTemplate :: H.Identifier -> Context a -> Item a -> Compiler (Item String)
loadAndApplyTemplate id' context item = do
  template <- H.loadBody id'
  applyTemplate template context item

applyBlocks :: Context a -> [Block] -> Item a -> Compiler String
applyBlocks context (block : rest) item = do
  thisResult <- applyBlock context block item
  restResults <- applyBlocks context rest item
  return (thisResult ++ restResults)
applyBlocks _ [] _ = return ""

applyBlock :: Context a -> Block -> Item a -> Compiler String
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

eval :: Context a -> Expression -> Item a -> Compiler (ContextValue a)
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
  NameExpression name _ -> unContext context name item
  --
  AccessExpression targetExp fieldExp pos ->
    eval context targetExp item >>= \case
      ContextValue target ->
        case fieldExp of
          NameExpression field' _ -> unContext target field' item
          _ ->
            eval context fieldExp item >>= \case
              StringValue field' -> unContext target field' item
              field' -> fail $ "Can't access field from context via " ++ getValueType field' ++ " in " ++ show pos
      ListValue list ->
        eval context fieldExp item >>= \case
          IntValue index
            | index < length list -> return $ list !! index
            | otherwise -> fail $ "Index " ++ show index ++ " out of bounds 0-" ++ show (length list) ++ " in " ++ show pos
          val -> fail $ "Can't index into list with " ++ getValueType val ++ " in " ++ show pos
      target -> fail $ "Can't access field from " ++ getValueType target ++ " in " ++ show pos

apply :: Expression -> Expression -> SourcePos -> Context a -> Item a -> Compiler (ContextValue a)
apply fExp xExp pos context item =
  eval context fExp item >>= \case
    FunctionValue f' -> do
      x <- eval context xExp item
      f' x context item
    x -> fail $ "Can't apply " ++ show x ++ " as function in " ++ show pos
