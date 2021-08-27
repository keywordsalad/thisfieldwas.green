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
  deriving stock (Eq, Show)

instance AstStructure Template Template' where
  intoAstStructure (Template blocks' _) = Template' (intoAstStructure blocks')

data ApplyBlock' = ApplyBlock' Expression' [Block']
  deriving stock (Eq, Show)

instance AstStructure ApplyBlock ApplyBlock' where
  intoAstStructure (ApplyBlock e bs _) = ApplyBlock' (intoAstStructure e) (intoAstStructure <$> bs)

data DefaultBlock' = DefaultBlock' [Block']
  deriving stock (Eq, Show)

instance AstStructure DefaultBlock DefaultBlock' where
  intoAstStructure (DefaultBlock bs _) = DefaultBlock' (intoAstStructure <$> bs)

data Block'
  = TextBlock' String
  | ExpressionBlock' Expression'
  | CommentBlock' String
  | ChromeBlock' Expression' [Block']
  | AltBlock' [ApplyBlock'] (Maybe DefaultBlock')
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show)

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
  | IncludeBlockToken'
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
  | BoolToken' Bool
  | NameToken' String
  | StringToken' String
  | IntToken' Int
  | DoubleToken' Double
  | TextToken' String
  | TurnOffToken'
  | TurnOnToken'
  deriving stock (Eq, Show)

instance AstStructure Token Token' where
  intoAstStructure = \case
    ExpressionBlockToken _ -> ExpressionBlockToken'
    CommentBlockToken _ -> CommentBlockToken'
    IncludeBlockToken _ -> IncludeBlockToken'
    AltBlockToken _ -> AltBlockToken'
    ChromeBlockToken _ -> ChromeBlockToken'
    CloseBlockToken _ -> CloseBlockToken'
    OpenParenToken _ -> OpenParenToken'
    CloseParenToken _ -> CloseParenToken'
    OpenBracketToken _ -> OpenBracketToken'
    CloseBracketToken _ -> CloseBracketToken'
    OpenBraceToken _ -> OpenBraceToken'
    CloseBraceToken _ -> CloseBraceToken'
    PipeToken _ -> PipeToken'
    CommaToken _ -> CommaToken'
    DotToken _ -> DotToken'
    ColonToken _ -> ColonToken'
    EndToken _ -> EndToken'
    ElseToken _ -> ElseToken'
    BoolToken b _ -> BoolToken' b
    NameToken n _ -> NameToken' n
    StringToken s _ -> StringToken' s
    IntToken i _ -> IntToken' i
    DoubleToken d _ -> DoubleToken' d
    TextToken t _ -> TextToken' t
    TurnOffToken _ -> TurnOffToken'
