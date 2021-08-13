module Green.Template.Ast where

import Data.Binary
import Data.List.NonEmpty
import GHC.Generics
import Text.Parsec hiding (getPosition)
import Text.Parsec.Pos

data Template = Template [Block] SourcePos
  deriving stock (Show, Generic)

getTemplatePos :: Template -> SourcePos
getTemplatePos (Template _ pos) = pos

instance Binary Template where
  get = do
    f <- Template <$> get
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put (Template blocks pos) = do
    put blocks
    put $ BinaryPos pos

data Block
  = TextBlock String SourcePos -- Any text, like this.
  | ExpressionBlock Expression SourcePos -- {{ interpolated thing }}
  | CommentBlock String SourcePos -- {{! this comment }}
  | LayoutBlock Expression SourcePos -- {{@ include "this/file.md" }}
  | TemplateStartBlock Expression SourcePos -- {{# expression }}
  | TemplateNextBlock Expression SourcePos -- {{ else expression }}
  | TemplateElseBlock SourcePos -- {{ else }}
  | TemplateEndBlock SourcePos -- {{ end }}
  | LayoutApplyBlock Expression Template SourcePos
  | TemplateBlock (NonEmpty TemplateApplyBlock) (Maybe TemplateDefaultBlock) SourcePos
  deriving stock (Show, Generic)

getBlockPos :: Block -> SourcePos
getBlockPos = \case
  TextBlock _ pos -> pos
  ExpressionBlock _ pos -> pos
  CommentBlock _ pos -> pos
  LayoutBlock _ pos -> pos
  TemplateStartBlock _ pos -> pos
  TemplateNextBlock _ pos -> pos
  TemplateElseBlock pos -> pos
  TemplateEndBlock pos -> pos
  LayoutApplyBlock _ _ pos -> pos
  TemplateBlock _ _ pos -> pos

getBlockTag :: Block -> Int
getBlockTag = \case
  TextBlock {} -> 1
  ExpressionBlock {} -> 2
  CommentBlock {} -> 3
  LayoutBlock {} -> 4
  TemplateStartBlock {} -> 5
  TemplateNextBlock {} -> 6
  TemplateElseBlock {} -> 7
  TemplateEndBlock {} -> 8
  LayoutApplyBlock {} -> 9
  TemplateBlock {} -> 10

instance Binary Block where
  get = do
    tag <- get :: Get Int
    f <- case tag of
      1 -> TextBlock <$> get
      2 -> ExpressionBlock <$> get
      3 -> CommentBlock <$> get
      4 -> LayoutBlock <$> get
      5 -> TemplateStartBlock <$> get
      6 -> TemplateNextBlock <$> get
      7 -> pure TemplateElseBlock
      8 -> pure TemplateEndBlock
      9 -> LayoutApplyBlock <$> get <*> get
      10 -> TemplateBlock <$> get <*> get
      _ -> error $ "Unrecognized block tag " ++ show tag
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put block = do
    put $ getBlockTag block
    case block of
      TextBlock text _ -> put text
      ExpressionBlock expression _ -> put expression
      CommentBlock text _ -> put text
      LayoutBlock expression _ -> put expression
      TemplateStartBlock expression _ -> put expression
      TemplateNextBlock expression _ -> put expression
      TemplateElseBlock _ -> return ()
      TemplateEndBlock _ -> return ()
      LayoutApplyBlock expression blocks _ -> put expression >> put blocks
      TemplateBlock blocks default' _ -> put blocks >> put default'
    put $ BinaryPos (getBlockPos block)

newtype BinaryPos = BinaryPos SourcePos
  deriving stock (Show)

unBinaryPos :: BinaryPos -> SourcePos
unBinaryPos (BinaryPos pos) = pos

instance Binary BinaryPos where
  get = BinaryPos <$> (newPos <$> get <*> get <*> get)
  put binaryPos = do
    let pos = unBinaryPos binaryPos
    put $ sourceName pos
    put $ sourceLine pos
    put $ sourceColumn pos

data TemplateApplyBlock = TemplateApplyBlock Expression Template SourcePos
  deriving stock (Show, Generic)

getTemplateApplyPos :: TemplateApplyBlock -> SourcePos
getTemplateApplyPos (TemplateApplyBlock _ _ pos) = pos

instance Binary TemplateApplyBlock where
  get = do
    f <- TemplateApplyBlock <$> get <*> get
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put (TemplateApplyBlock expression blocks pos) = do
    put expression
    put blocks
    put $ BinaryPos pos

data TemplateDefaultBlock = TemplateDefaultBlock Template SourcePos
  deriving stock (Show, Generic)

getTemplateDefaultPos :: TemplateDefaultBlock -> SourcePos
getTemplateDefaultPos (TemplateDefaultBlock _ pos) = pos

instance Binary TemplateDefaultBlock where
  get = do
    f <- TemplateDefaultBlock <$> get
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put (TemplateDefaultBlock blocks pos) = do
    put blocks
    put $ BinaryPos pos

data Expression
  = NameExpression String SourcePos -- name, id
  | StringExpression String SourcePos -- "a string of text"
  | IntExpression Int SourcePos -- 123, 456, 2
  | DoubleExpression Double SourcePos -- 0.2, 1.45
  | BoolExpression Bool SourcePos -- true, false
  | ApplyExpression Expression Expression SourcePos -- fn arg
  | AccessExpression Expression Expression SourcePos -- target.field
  | FilterExpression Expression Expression SourcePos -- arg | fn
  | ContextExpression [(String, Expression)] SourcePos -- { name0: value0, name1: value1 }
  | ListExpression [Expression] SourcePos -- [a, b, c]
  deriving stock (Show, Generic)

getExpressionPos :: Expression -> SourcePos
getExpressionPos = \case
  NameExpression _ pos -> pos
  StringExpression _ pos -> pos
  IntExpression _ pos -> pos
  DoubleExpression _ pos -> pos
  BoolExpression _ pos -> pos
  ApplyExpression _ _ pos -> pos
  AccessExpression _ _ pos -> pos
  FilterExpression _ _ pos -> pos
  ContextExpression _ pos -> pos
  ListExpression _ pos -> pos

getExpressionTag :: Expression -> Int
getExpressionTag = \case
  NameExpression {} -> 1
  StringExpression {} -> 2
  IntExpression {} -> 3
  DoubleExpression {} -> 4
  BoolExpression {} -> 5
  ApplyExpression {} -> 6
  AccessExpression {} -> 7
  FilterExpression {} -> 8
  ContextExpression {} -> 9
  ListExpression {} -> 10

instance Binary Expression where
  get = do
    tag <- get :: Get Int
    f <- case tag of
      1 -> NameExpression <$> get
      2 -> StringExpression <$> get
      3 -> IntExpression <$> get
      4 -> DoubleExpression <$> get
      5 -> BoolExpression <$> get
      6 -> ApplyExpression <$> get <*> get
      7 -> AccessExpression <$> get <*> get
      8 -> FilterExpression <$> get <*> get
      9 -> ContextExpression <$> get
      10 -> ListExpression <$> get
      _ -> error $ "Unrecognized expression tag " ++ show tag
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put expression = do
    put $ getExpressionTag expression
    case expression of
      NameExpression value _ -> put value
      StringExpression value _ -> put value
      IntExpression value _ -> put value
      DoubleExpression value _ -> put value
      BoolExpression value _ -> put value
      ApplyExpression fn arg _ -> put fn >> put arg
      AccessExpression target field _ -> put target >> put field
      FilterExpression arg fn _ -> put arg >> put fn
      ContextExpression pairs _ -> put pairs
      ListExpression values _ -> put values
    put $ BinaryPos (getExpressionPos expression)
