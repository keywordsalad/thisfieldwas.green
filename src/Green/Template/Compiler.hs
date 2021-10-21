module Green.Template.Compiler where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.List.NonEmpty
import Green.Common
import Green.Template.Ast
import Green.Template.Context hiding (field)
import Green.Template.Source.Parser (parse)
import Hakyll.Core.Compiler.Internal

-- | Compiles an item as a template.
templateCompiler :: Compiler (Item Template)
templateCompiler =
  cached "Green.Template.Compiler.templateCompiler" $
    getResourceBody
      >>= compileTemplateItem
      >>= makeItem

-- | Takes an item and compiles a template from it.
compileTemplateItem :: Item String -> Compiler Template
compileTemplateItem item = do
  let filePath = toFilePath $ itemIdentifier item
  either (fail . show) return $ parse filePath (itemBody item)

loadTemplate' :: Identifier -> TemplateRunner a Template
loadTemplate' = lift . fmap itemBody . load

loadAndApplyTemplate :: Identifier -> Context String -> Item String -> Compiler (Item String)
loadAndApplyTemplate id' context item = do
  let s = templateRunner context item
  evalStateT (loadAndApplyTemplate' id') s

loadAndApplyTemplate' :: Identifier -> TemplateRunner String (Item String)
loadAndApplyTemplate' = lift . makeItem <=< applyTemplate' <=< loadTemplate'

-- | Applies an item as a template to itself.
applyAsTemplate :: Context String -> Item String -> Compiler (Item String)
applyAsTemplate context item = do
  template <- compileTemplateItem item
  applyTemplate template context item

-- | Applies a template with context to an item
applyTemplate :: Template -> Context String -> Item String -> Compiler (Item String)
applyTemplate template context item = do
  let s = templateRunner context item
  makeItem =<< evalStateT (applyTemplate' template) s

applyTemplate' :: Template -> TemplateRunner String String
applyTemplate' (Template bs src) =
  tplWithCall ("template " ++ src) (reduceBlocks bs)

reduceBlocks :: [Block] -> TemplateRunner String String -- Context String -> [Block] -> Item String -> Compiler String
reduceBlocks = stringify . intoValue <=< applyBlocks

applyBlocks :: [Block] -> TemplateRunner String [ContextValue String]
applyBlocks = mapM applyBlock

applyBlock :: Block -> TemplateRunner String (ContextValue String)
applyBlock = \case
  TextBlock t _ -> return $ intoValue t
  ExpressionBlock e _ -> eval e
  CommentBlock {} -> return EmptyValue
  ChromeBlock e bs _ -> intoValue <$> applyGuard e bs [] Nothing
  AltBlock (ApplyBlock e bs _ :| alts) mdef _ -> intoValue <$> applyGuard e bs alts mdef
  where
    applyGuard e bs alts mdef =
      eval e >>= \case
        FunctionValue f ->
          pure <$> f (intoValue bs)
        x -> do
          truthy <- isTruthy x
          if truthy
            then applyBlocks bs
            else applyAlt alts mdef
    --
    applyAlt (ApplyBlock e bs _ : alts) mdef = applyGuard e bs alts mdef
    applyAlt _ (Just (DefaultBlock bs _)) = applyBlocks bs
    applyAlt _ Nothing = return []

eval :: Expression -> TemplateRunner a (ContextValue a)
eval = \case
  NameExpression name _ -> do
    context <- tplContext
    item <- tplItem
    trace <- tplTrace
    s <- get
    (result, s') <-
      lift $
        runStateT (unContext context name) s `compilerCatch` \e ->
          let msg = "Failed to resolve field " ++ show name ++ " from item context for " ++ itemFilePath item ++ ", trace: [" ++ intercalate ", " trace ++ "]"
           in compilerThrow (msg : compilerErrorMessages e)
    put s'
    return result
  StringExpression s _ -> return $ intoValue s
  IntExpression n _ -> return $ intoValue n
  DoubleExpression x _ -> return $ intoValue x
  BoolExpression b _ -> return $ intoValue b
  ApplyExpression f x _ -> apply f x
  AccessExpression target field pos ->
    eval target >>= \case
      ContextValue target' ->
        eval field
          >>= \case
            StringValue name -> return name
            x -> tplFail $ "invalid field " ++ show x ++ " near " ++ show (getExpressionPos field)
          >>= unContext target'
      x -> tplFail $ "invalid context " ++ show x ++ " near " ++ show pos
  FilterExpression x f _ -> apply f x
  ContextExpression pairs _ -> do
    pairs' <- mapM (sequence . second eval) pairs
    return $ ContextValue (intoContext pairs')
  ListExpression xs _ -> intoValue <$> mapM eval xs
  where
    apply f x =
      eval f >>= \case
        FunctionValue f' -> f' (ThunkValue $ eval x)
        x' -> fail $ "invalid function " ++ show x' ++ " in " ++ show (getExpressionPos f)

stringify :: ContextValue String -> TemplateRunner String String
stringify = \case
  EmptyValue -> return ""
  ContextValue {} -> tplFail "can't stringify context"
  ListValue xs -> mconcat <$> mapM stringify xs
  BoolValue b -> return $ show b
  StringValue s -> return s
  DoubleValue x -> return $ show x
  IntValue n -> return $ show n
  FunctionValue {} -> tplFail "can't stringify function"
  BlockValue block -> case block of
    TextBlock t _ -> return t
    CommentBlock {} -> return ""
    ExpressionBlock e _ -> stringify =<< eval e
    _ -> stringify =<< applyBlock block
  ItemValue _ items -> return $ mconcat $ itemBody <$> items
  ThunkValue fx -> stringify =<< force =<< fx

isTruthy :: ContextValue a -> TemplateRunner a Bool
isTruthy = \case
  EmptyValue -> return False
  ContextValue {} -> return True
  ListValue xs -> return $ not (null xs)
  BoolValue x -> return x
  StringValue x -> return $ not (null x)
  DoubleValue x -> return $ x /= 0
  IntValue x -> return $ x /= 0
  FunctionValue {} -> return True
  BlockValue {} -> return True
  ItemValue _ xs -> return $ not (null xs)
  ThunkValue fx -> isTruthy =<< force =<< fx

force :: ContextValue a -> TemplateRunner a (ContextValue a)
force = \case
  ThunkValue fx -> do
    force =<< fx
  x -> return x
