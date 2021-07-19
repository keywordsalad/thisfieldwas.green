module Green.Template.Data where

import Data.Binary
import Prelude hiding (lookup)

newtype Template = Template [Block] deriving stock (Show)

instance Binary Template where
  get = Template <$> get
  put (Template blocks) = put blocks

data Block
  = TextBlock String
  | CommentBlock String
  | TemplateBlock (Expression, Template) [(Expression, Template)] Template
  | ExpressionBlock Expression
  deriving stock (Show)

instance Binary Block where
  get =
    (get :: Get Int) >>= \case
      1 -> TextBlock <$> get
      2 -> CommentBlock <$> get
      3 -> TemplateBlock <$> get <*> get <*> get
      4 -> ExpressionBlock <$> get
      n -> error $ "Failed to get block from tag " ++ show n
  put block =
    put (blockTag block) >> case block of
      TextBlock text -> put text
      CommentBlock text -> put text
      TemplateBlock expression apply unapply -> put expression >> put apply >> put unapply
      ExpressionBlock expression -> put expression

blockTag :: Block -> Int
blockTag = \case
  TextBlock {} -> 1
  CommentBlock {} -> 2
  TemplateBlock {} -> 3
  ExpressionBlock {} -> 4

data Expression
  = NameExpression String
  | StringExpression String
  | IntExpression Int
  | DoubleExpression Double
  | BoolExpression Bool
  | ApplyExpression Expression Expression
  | AccessExpression Expression Expression
  | ContextExpression [(String, Expression)]
  deriving stock (Show)

instance Binary Expression where
  get =
    (get :: Get Int) >>= \case
      1 -> NameExpression <$> get
      2 -> StringExpression <$> get
      3 -> IntExpression <$> get
      4 -> DoubleExpression <$> get
      5 -> BoolExpression <$> get
      6 -> ApplyExpression <$> get <*> get
      7 -> AccessExpression <$> get <*> get
      8 -> ContextExpression <$> get
      n -> error $ "Failed to get expression from tag " ++ show n
  put expression =
    put (expressionTag expression) >> case expression of
      NameExpression value -> put value
      StringExpression value -> put value
      IntExpression value -> put value
      DoubleExpression value -> put value
      BoolExpression value -> put value
      ApplyExpression f x -> put f >> put x
      AccessExpression record field -> put record >> put field
      ContextExpression pairs -> put pairs

expressionTag :: Expression -> Int
expressionTag = \case
  NameExpression {} -> 1
  StringExpression {} -> 2
  IntExpression {} -> 3
  DoubleExpression {} -> 4
  BoolExpression {} -> 5
  ApplyExpression {} -> 6
  AccessExpression {} -> 7
  ContextExpression {} -> 8
