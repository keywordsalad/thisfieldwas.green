module Green.Template.ApproxAst where

import Data.Bifunctor
import Data.List.NonEmpty
import Green.Template

-- | Converts type @a@ into a type @b@ which is _approximately_ equivalent to
-- type @a@.
--
-- Specifically this class is used to convert from the concrete AST emitted by
-- parsers into an AST omitting meta and source information so that asserting
-- equivalence in tests against structures emitted by parsers is a lot easier.
class ApproxAst a b where
  toApprox :: a -> b

instance (Functor f, ApproxAst a b) => ApproxAst (f a) (f b) where
  toApprox = fmap toApprox

newtype Template' = Template' [Block']
  deriving stock (Eq, Show)

instance ApproxAst Template Template' where
  toApprox (Template blocks' _) = Template' (toApprox blocks')

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

instance ApproxAst Block Block' where
  toApprox = \case
    TemplateElseBlock _ -> TemplateElseBlock'
    TemplateEndBlock _ -> TemplateEndBlock'
    TextBlock x _ -> TextBlock' x
    CommentBlock x _ -> CommentBlock' x
    ExpressionBlock x _ -> ExpressionBlock' (toApprox x)
    LayoutBlock x _ -> LayoutBlock' (toApprox x)
    TemplateStartBlock x _ -> TemplateStartBlock' (toApprox x)
    TemplateNextBlock x _ -> TemplateNextBlock' (toApprox x)
    LayoutApplyBlock x xs _ -> LayoutApplyBlock' (toApprox x) (toApprox xs)
    TemplateBlock xs x _ -> TemplateBlock' (toApprox xs) (toApprox x)

data TemplateApplyBlock' = TemplateApplyBlock' Expression' Template'
  deriving stock (Eq, Show)

instance ApproxAst TemplateApplyBlock TemplateApplyBlock' where
  toApprox (TemplateApplyBlock x t _) = TemplateApplyBlock' (toApprox x) (toApprox t)

newtype TemplateDefaultBlock' = TemplateDefaultBlock' Template'
  deriving stock (Eq, Show)

instance ApproxAst TemplateDefaultBlock TemplateDefaultBlock' where
  toApprox (TemplateDefaultBlock t _) = TemplateDefaultBlock' (toApprox t)

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

instance ApproxAst Expression Expression' where
  toApprox = \case
    NameExpression x _ -> NameExpression' x
    StringExpression x _ -> StringExpression' x
    IntExpression x _ -> IntExpression' x
    DoubleExpression x _ -> DoubleExpression' x
    BoolExpression x _ -> BoolExpression' x
    ApplyExpression f x _ -> ApplyExpression' (toApprox f) (toApprox x)
    AccessExpression t f _ -> AccessExpression' (toApprox t) (toApprox f)
    FilterExpression x f _ -> FilterExpression' (toApprox x) (toApprox f)
    ContextExpression xs _ -> ContextExpression' (second toApprox <$> xs)

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

instance ApproxAst Token Token' where
  toApprox = \case
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
