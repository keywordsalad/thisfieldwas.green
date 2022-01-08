module Green.Template.AstStructure where

import Data.Bifunctor
import qualified Data.List.NonEmpty as NEL
import Green.Template
import Green.Template.Source.Lexer

-- | Converts type @a@ into a type @b@ which is _structurally_ equivalent to
-- type @a@.
--
-- Specifically this class is used to convert from the concrete AST emitted by
-- parsers into an AST omitting meta and source information so that asserting
-- equivalence in tests against structure is much easier.
class AstStructure a b where
  intoAstStructure :: a -> b

instance (Functor f, AstStructure a b) => AstStructure (f a) (f b) where
  intoAstStructure = fmap intoAstStructure

newtype Template' = Template' [Block']
  deriving (Eq, Show)

instance AstStructure Template Template' where
  intoAstStructure (Template blocks' _) = Template' (intoAstStructure blocks')

data ApplyBlock' = ApplyBlock' Expression' [Block']
  deriving (Eq, Show)

instance AstStructure ApplyBlock ApplyBlock' where
  intoAstStructure (ApplyBlock e bs _) = ApplyBlock' (intoAstStructure e) (intoAstStructure <$> bs)

data DefaultBlock' = DefaultBlock' [Block']
  deriving (Eq, Show)

instance AstStructure DefaultBlock DefaultBlock' where
  intoAstStructure (DefaultBlock bs _) = DefaultBlock' (intoAstStructure <$> bs)

data Block'
  = TextBlock' String
  | ExpressionBlock' Expression'
  | CommentBlock' String
  | ChromeBlock' Expression' [Block']
  | AltBlock' [ApplyBlock'] (Maybe DefaultBlock')
  deriving (Eq, Show)

instance AstStructure Block Block' where
  intoAstStructure = \case
    TextBlock t _ -> TextBlock' t
    ExpressionBlock e _ -> ExpressionBlock' (intoAstStructure e)
    CommentBlock t _ -> CommentBlock' t
    ChromeBlock e bs _ -> ChromeBlock' (intoAstStructure e) (intoAstStructure <$> bs)
    AltBlock alts def _ -> AltBlock' (NEL.toList $ intoAstStructure <$> alts) (intoAstStructure <$> def)

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
  | ListExpression' [Expression']
  deriving (Eq, Show)

instance AstStructure Expression Expression' where
  intoAstStructure = \case
    NameExpression x _ -> NameExpression' x
    StringExpression x _ -> StringExpression' x
    IntExpression x _ -> IntExpression' x
    DoubleExpression x _ -> DoubleExpression' x
    BoolExpression x _ -> BoolExpression' x
    ApplyExpression f x _ -> ApplyExpression' (intoAstStructure f) (intoAstStructure x)
    AccessExpression t f _ -> AccessExpression' (intoAstStructure t) (intoAstStructure f)
    FilterExpression x f _ -> FilterExpression' (intoAstStructure x) (intoAstStructure f)
    ContextExpression xs _ -> ContextExpression' (second intoAstStructure <$> xs)
    ListExpression xs _ -> ListExpression' (intoAstStructure <$> xs)

data Token'
  = ExpressionBlockToken'
  | CommentBlockToken'
  | AltBlockToken'
  | ChromeBlockToken'
  | CloseBlockToken'
  | OpenParenToken'
  | CloseParenToken'
  | OpenBracketToken'
  | CloseBracketToken'
  | OpenBraceToken'
  | CloseBraceToken'
  | PipeToken'
  | CommaToken'
  | DotToken'
  | ColonToken'
  | EndToken'
  | ElseToken'
  | TurnOffToken'
  | BoolToken' Bool
  | NameToken' String
  | StringToken' String
  | IntToken' Int
  | DoubleToken' Double
  | TextToken' String
  deriving (Eq, Show)

instance AstStructure Token Token' where
  intoAstStructure = \case
    TaggedToken t _ -> case t of
      ExpressionBlockToken -> ExpressionBlockToken'
      CommentBlockToken -> CommentBlockToken'
      AltBlockToken -> AltBlockToken'
      ChromeBlockToken -> ChromeBlockToken'
      CloseBlockToken -> CloseBlockToken'
      OpenParenToken -> OpenParenToken'
      CloseParenToken -> CloseParenToken'
      OpenBracketToken -> OpenBracketToken'
      CloseBracketToken -> CloseBracketToken'
      OpenBraceToken -> OpenBraceToken'
      CloseBraceToken -> CloseBraceToken'
      PipeToken -> PipeToken'
      CommaToken -> CommaToken'
      DotToken -> DotToken'
      ColonToken -> ColonToken'
      EndToken -> EndToken'
      ElseToken -> ElseToken'
      TurnOffToken -> TurnOffToken'
    BoolToken b _ -> BoolToken' b
    NameToken n _ -> NameToken' n
    StringToken s _ -> StringToken' s
    IntToken n _ -> IntToken' n
    DoubleToken d _ -> DoubleToken' d
    TextToken t _ -> TextToken' t
