module Green.Template.Compiler where

import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import Green.Template.Ast
import Green.Template.Context hiding (field)
import Green.Template.Source.Parser (parse)
import Hakyll (Compiler, Item)
import qualified Hakyll as H
import Prelude hiding (lookup)

-- | Compiles an item as a template.
templateCompiler :: Compiler (Item Template)
templateCompiler =
  H.cached "Green.Template.Compiler.templateCompiler" $
    H.getResourceBody
      >>= compileTemplateItem
      >>= H.makeItem

-- | Takes an item and compiles a template from it.
compileTemplateItem :: Item String -> Compiler Template
compileTemplateItem item = do
  let filePath = H.toFilePath $ H.itemIdentifier item
  either (fail . show) return $ parse filePath (H.itemBody item)

loadTemplate :: H.Identifier -> Compiler (Item Template)
loadTemplate = H.load

loadTemplateBody :: H.Identifier -> Compiler Template
loadTemplateBody id' = H.itemBody <$> loadTemplate id'

-- | Applies an item as a template to itself.
applyAsTemplate :: Context String -> Item String -> Compiler (Item String)
applyAsTemplate context item = do
  template <- compileTemplateItem item
  applyTemplate template context item

loadAndApplyTemplate :: H.Identifier -> Context String -> Item String -> Compiler (Item String)
loadAndApplyTemplate id' context item = do
  template <- loadTemplateBody id'
  applyTemplate template context item

-- | Applies a template with context to an item
applyTemplate :: Template -> Context String -> Item String -> Compiler (Item String)
applyTemplate (Template bs _) context item = do
  result <- reduceBlocks context bs item
  return $ H.itemSetBody result item

reduceBlocks :: Context String -> [Block] -> Item String -> Compiler String
reduceBlocks context bs item = do
  values <- applyBlocks context bs item
  stringify context item . intoValue $ values

applyBlocks :: Context String -> [Block] -> Item String -> Compiler [ContextValue String]
applyBlocks context bs item = mapM applyBlock' bs
  where
    applyBlock' block = applyBlock context block item

applyBlock :: Context String -> Block -> Item String -> Compiler (ContextValue String)
applyBlock context block item = case block of
  TextBlock t _ -> return $ intoValue t
  ExpressionBlock e _ -> eval context e item
  CommentBlock {} -> return EmptyValue
  ChromeBlock e bs _ -> intoValue <$> applyGuard e bs [] Nothing
  AltBlock (ApplyBlock e bs _ :| alts) mdef _ -> intoValue <$> applyGuard e bs alts mdef
  where
    applyGuard e bs alts mdef =
      eval context e item >>= \case
        FunctionValue f ->
          pure <$> f (intoValue bs) context item
        x -> do
          truthy <- isTruthy x
          if truthy
            then applyBlocks context bs item
            else applyAlt alts mdef
    --
    applyAlt (ApplyBlock e bs _ : alts) mdef = applyGuard e bs alts mdef
    applyAlt _ (Just (DefaultBlock bs _)) = applyBlocks context bs item
    applyAlt _ Nothing = return []

eval :: Context String -> Expression -> Item String -> Compiler (ContextValue String)
eval context e item = case e of
  NameExpression key _ -> unContext context key item
  StringExpression s _ -> return $ intoValue s
  IntExpression n _ -> return $ intoValue n
  DoubleExpression x _ -> return $ intoValue x
  BoolExpression b _ -> return $ intoValue b
  ApplyExpression f x _ -> apply f x
  AccessExpression target field pos ->
    eval context target item >>= \case
      ContextValue target' -> do
        field' <-
          eval context field item >>= \case
            StringValue name -> return name
            x -> fail $ "Invalid field " ++ show x ++ " near " ++ show (getExpressionPos field)
        unContext target' field' item
      x -> fail $ "Invalid context " ++ show x ++ " near " ++ show pos
  FilterExpression x f _ -> apply f x
  ContextExpression pairs _ -> do
    pairs' <- sequence (sequence . second (\x -> eval context x item) <$> pairs)
    return $ ContextValue (intoContext pairs')
  ListExpression xs _ -> intoValue <$> mapM (\x -> eval context x item) xs
  where
    apply f x =
      eval context f item >>= \case
        FunctionValue f' -> f' (ThunkValue (eval context x item)) context item
        x' -> fail $ "Invalid function " ++ show x' ++ " in " ++ show (getExpressionPos f)

stringify :: Context String -> Item String -> ContextValue String -> Compiler String
stringify context item value = case value of
  EmptyValue -> return ""
  UndefinedValue name -> fail $ "Undefined name: " ++ show name
  ContextValue {} -> fail "Can't stringify context"
  ListValue xs -> mconcat <$> mapM (stringify context item) xs
  BoolValue b -> return $ show b
  StringValue s -> return s
  DoubleValue x -> return $ show x
  IntValue n -> return $ show n
  FunctionValue {} -> fail "Can't stringify function"
  BlockValue block -> case block of
    TextBlock t _ -> return t
    CommentBlock {} -> return ""
    ExpressionBlock e _ -> stringify context item =<< eval context e item
    _ -> stringify context item =<< applyBlock context block item
  ItemValue i -> return $ H.itemBody i
  ThunkValue fx -> stringify context item =<< force =<< fx

isTruthy :: ContextValue a -> Compiler Bool
isTruthy = \case
  EmptyValue -> return False
  UndefinedValue {} -> return False
  ContextValue {} -> return True
  ListValue xs -> return $ not (null xs)
  BoolValue x -> return x
  StringValue x -> return $ not (null x)
  DoubleValue x -> return $ x /= 0
  IntValue x -> return $ x /= 0
  FunctionValue {} -> return True
  BlockValue {} -> return True
  ItemValue {} -> return True
  ThunkValue fx -> isTruthy =<< force =<< fx

force :: ContextValue a -> Compiler (ContextValue a)
force = \case
  ThunkValue fx -> force =<< fx
  x -> return x
