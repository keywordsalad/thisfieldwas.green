module Green.Template.Structural where

import Data.Bifunctor
import Data.List.NonEmpty
import Green.Template

-- | Converts type @a@ into a type @b@ which is _structurally_ equivalent to
-- type @a@.
--
-- Specifically this class is used to convert from the concrete AST emitted by
-- parsers into an AST omitting meta and source information so that asserting
-- equivalence in tests against structure is much easier.
class Structural a b where
  toStructure :: a -> b

instance (Functor f, Structural a b) => Structural (f a) (f b) where
  toStructure = fmap toStructure

newtype Template' = Template' [Block']
  deriving stock (Eq, Show)

instance Structural Template Template' where
  toStructure (Template blocks' _) = Template' (toStructure blocks')

data Block'
  = TextBlock' String
  | ExpressionBlock' Expression'
  | CommentBlock' String
  | LayoutBlock' Expression'
  | TemplateStartBlock' Expression'
  | TemplateNextBlock' Expression'
  | TemplateElseBlock'
  | TemplateEndBlock'
  | LayoutApplyBlock' Expression' Template'
  | TemplateBlock' (NonEmpty TemplateApplyBlock') (Maybe TemplateDefaultBlock')
  deriving stock (Eq, Show)

instance Structural Block Block' where
  toStructure = \case
    TemplateElseBlock _ -> TemplateElseBlock'
    TemplateEndBlock _ -> TemplateEndBlock'
    TextBlock x _ -> TextBlock' x
    CommentBlock x _ -> CommentBlock' x
    ExpressionBlock x _ -> ExpressionBlock' (toStructure x)
    LayoutBlock x _ -> LayoutBlock' (toStructure x)
    TemplateStartBlock x _ -> TemplateStartBlock' (toStructure x)
    TemplateNextBlock x _ -> TemplateNextBlock' (toStructure x)
    LayoutApplyBlock x xs _ -> LayoutApplyBlock' (toStructure x) (toStructure xs)
    TemplateBlock xs x _ -> TemplateBlock' (toStructure xs) (toStructure x)

data TemplateApplyBlock' = TemplateApplyBlock' Expression' Template'
  deriving stock (Eq, Show)

instance Structural TemplateApplyBlock TemplateApplyBlock' where
  toStructure (TemplateApplyBlock x t _) = TemplateApplyBlock' (toStructure x) (toStructure t)

newtype TemplateDefaultBlock' = TemplateDefaultBlock' Template'
  deriving stock (Eq, Show)

instance Structural TemplateDefaultBlock TemplateDefaultBlock' where
  toStructure (TemplateDefaultBlock t _) = TemplateDefaultBlock' (toStructure t)

data Expression'
  = NameExpression' String
  | StringExpression' String
  | IntExpression' Int
  | DoubleExpression' Double
  | BoolExpression' Bool
  | ApplyExpression' Expression' Expression'
  | AccessExpression' Expression' Expression'
  | FilterExpression' Expression' Expression'
  | ContextExpression' [(String, Expression')]
  deriving stock (Eq, Show)

instance Structural Expression Expression' where
  toStructure = \case
    NameExpression x _ -> NameExpression' x
    StringExpression x _ -> StringExpression' x
    IntExpression x _ -> IntExpression' x
    DoubleExpression x _ -> DoubleExpression' x
    BoolExpression x _ -> BoolExpression' x
    ApplyExpression f x _ -> ApplyExpression' (toStructure f) (toStructure x)
    AccessExpression t f _ -> AccessExpression' (toStructure t) (toStructure f)
    FilterExpression x f _ -> FilterExpression' (toStructure x) (toStructure f)
    ContextExpression xs _ -> ContextExpression' (second toStructure <$> xs)

data Token'
  = OpenBlockToken'
  | CloseBlockToken'
  | OpenLayoutToken'
  | OpenTemplateToken'
  | OpenCommentToken'
  | OpenBraceToken'
  | CloseBraceToken'
  | OpenParenToken'
  | CloseParenToken'
  | PipeToken'
  | CommaToken'
  | DotToken'
  | ColonToken'
  | EndToken'
  | ElseToken'
  | BoolToken' Bool
  | NameToken' String
  | StringToken' String
  | IntToken' Int
  | DoubleToken' Double
  | TextToken' String
  deriving stock (Eq, Show)

instance Structural Token Token' where
  toStructure = \case
    OpenBlockToken _ -> OpenBlockToken'
    CloseBlockToken _ -> CloseBlockToken'
    OpenLayoutToken _ -> OpenLayoutToken'
    OpenTemplateToken _ -> OpenTemplateToken'
    OpenCommentToken _ -> OpenCommentToken'
    OpenBraceToken _ -> OpenBraceToken'
    CloseBraceToken _ -> CloseBraceToken'
    OpenParenToken _ -> OpenParenToken'
    CloseParenToken _ -> CloseParenToken'
    PipeToken _ -> PipeToken'
    CommaToken _ -> CommaToken'
    DotToken _ -> DotToken'
    ColonToken _ -> ColonToken'
    EndToken _ -> EndToken'
    ElseToken _ -> ElseToken'
    BoolToken value _ -> BoolToken' value
    NameToken value _ -> NameToken' value
    StringToken value _ -> StringToken' value
    IntToken value _ -> IntToken' value
    DoubleToken value _ -> DoubleToken' value
    TextToken value _ -> TextToken' value
