module Green.Template.Compiler where

import Control.Applicative (liftA3)
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.List.NonEmpty as NonEmpty
import Green.Common
import Green.Template.Ast
import Green.Template.Context hiding (field)
import Green.Template.Source.Parser (parse)
import Hakyll.Core.Compiler.Internal

-- | Compiles an item as a template.
getResourceTemplate :: Compiler (Item Template)
getResourceTemplate =
  getResourceBody
    >>= compileTemplateItem
    >>= makeItem

-- | Takes an item and compiles a template from it.
compileTemplateItem :: Item String -> Compiler Template
compileTemplateItem item = do
  let filePath = toFilePath $ itemIdentifier item
  either (fail . show) return $ parse filePath (itemBody item)

loadTemplate :: Identifier -> TemplateRunner a Template
loadTemplate = lift . fmap itemBody . load

applyTemplate :: Identifier -> TemplateRunner String ()
applyTemplate =
  loadTemplate
    >=> reduceTemplate
    >=> lift . makeItem
    >=> tplPushItem

applyAsTemplate :: TemplateRunner String ()
applyAsTemplate =
  tplModifyItem do
    lift . compileTemplateItem
      >=> reduceTemplate
      >=> lift . makeItem

reduceTemplate :: Template -> TemplateRunner String String
reduceTemplate (Template bs src) =
  tplWithCall ("template " ++ src) (reduceBlocks bs)

reduceBlocks :: [Block] -> TemplateRunner String String
reduceBlocks = stringify . intoValue <=< applyBlocks

applyBlocks :: [Block] -> TemplateRunner String [ContextValue String]
applyBlocks = mapM applyBlock

applyBlock :: Block -> TemplateRunner String (ContextValue String)
applyBlock = tplWithPos getBlockPos \case
  TextBlock t _ -> return $ intoValue t
  ExpressionBlock e _ -> eval e
  CommentBlock {} -> return EmptyValue
  ChromeBlock e bs pos ->
    fmap intoValue do
      bs' <- reduceBlocks bs
      eval e >>= \case
        FunctionValue f -> f (intoValue bs')
        x -> tplFail $ "invalid chrome function " ++ show x ++ " near " ++ show pos
  AltBlock (ApplyBlock e bs _ :| alts) mdef _ ->
    intoValue <$> applyAltBlock e bs alts mdef

applyAltBlock ::
  Expression ->
  [Block] ->
  [ApplyBlock] ->
  Maybe DefaultBlock ->
  TemplateRunner String [ContextValue String]
applyAltBlock guard' bs alts mdef =
  eval guard' >>= \case
    FunctionValue f -> pure <$> f (intoValue bs)
    x ->
      isTruthy x >>= \case
        True -> applyBlocks bs
        False -> case alts of
          ApplyBlock guard'' bs' _ : alts' -> applyAltBlock guard'' bs' alts' mdef
          [] -> case mdef of
            Just (DefaultBlock bs' _) -> applyBlocks bs'
            Nothing -> return []

eval :: Expression -> TemplateRunner a (ContextValue a)
eval = tplWithPos getExpressionPos \case
  NameExpression name pos -> do
    (context, item, trace) <-
      liftA3 (,,) tplContext tplItem tplTrace `catchError` \e -> do
        tplFail $ "Caught error in template: " ++ show e ++ " near " ++ show pos
    s <- get
    (x, s') <-
      lift $
        runStateT (unContext context name) s `compilerCatch` \case
          CompilationFailure ne -> compilerThrow (NonEmpty.toList ne)
          CompilationNoResult ss -> return (UndefinedValue name item (show pos : trace) ss, s) -- TODO figure out how to get state changes to persist if value comes back empty
    put s'
    return x
  StringExpression s _ -> return $ intoValue s
  IntExpression n _ -> return $ intoValue n
  DoubleExpression x _ -> return $ intoValue x
  BoolExpression b _ -> return $ intoValue b
  ApplyExpression f x _ -> apply f x
  AccessExpression target field pos ->
    eval target >>= \case
      ContextValue target' -> do
        name <-
          eval field >>= \case
            StringValue x -> return x
            x -> tplFail $ "invalid field " ++ show x ++ " near " ++ show (getExpressionPos field)
        s <- get
        (x, s') <-
          lift $
            runStateT (unContext target' name) s `compilerCatch` \case
              CompilationFailure errors -> compilerThrow (NonEmpty.toList errors)
              CompilationNoResult _ -> return (EmptyValue, s) -- TODO figure out how to get state changes to persist if value comes back empty
        put s'
        return x
      EmptyValue -> return EmptyValue
      UndefinedValue {} -> return EmptyValue
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
        x' -> tplFail $ "invalid function " ++ show x' ++ " in " ++ show (getExpressionPos f)

stringify :: ContextValue String -> TemplateRunner String String
stringify = \case
  EmptyValue -> return ""
  UndefinedValue name item trace errors -> tplFail $ "can't stringify undefined value " ++ show name ++ "\nin item context for " ++ itemFilePath item ++ "\ntrace=[\n\t" ++ intercalate ",\n\t" trace ++ "\n],\nsuppressed=[\n\t" ++ intercalate ",\n\t" errors ++ "\n]\n"
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
  ItemValue item -> return $ itemBody item
  ThunkValue fx -> stringify =<< force =<< fx
  PairValue (_, x) -> stringify x

isTruthy :: ContextValue a -> TemplateRunner a Bool
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
  ItemValue _ -> return True
  ThunkValue fx -> isTruthy =<< force =<< fx
  PairValue (_, x) -> isTruthy x

force :: ContextValue a -> TemplateRunner a (ContextValue a)
force = \case
  ThunkValue fx -> force =<< fx
  x -> return x
