module Green.Template.Source.Parser where

import Data.Functor
import Data.List.NonEmpty as NEL
import Data.Maybe
import Data.String
import Green.Template.Ast
import Green.Template.Source.Lexer
import Green.Template.Source.Util
import Text.Parsec hiding (parse, runParser, token, tokens, (<?>))
import qualified Text.Parsec as P
import Prelude hiding (lex)

type Parser a = Parsec [Token] ParserState a

parse :: SourceName -> String -> Either ParseError Template
parse origin input = do
  tokens <- lex origin input
  blocks <- runParser (many block <* eof) origin tokens
  return $ Template blocks origin

block :: Parser Block
block =
  tryOne
    [ offBlock,
      commentBlock,
      chromeBlock,
      altBlock,
      expressionBlock,
      textBlock
    ]

commentBlock :: Parser Block
commentBlock = labeled "CommentBlock" $
  withPosition do
    -- {{!
    withTag CommentBlockToken
    -- ...text...
    value <- withToken \case
      TextToken value _ -> Just value
      _ -> Nothing
    -- }}
    closeBlock
    return $ CommentBlock value

offBlock :: Parser Block
offBlock = labeled "OffBlock" $
  withPosition do
    -- {{*
    withTag TurnOffToken
    -- ...text...
    value <- withToken \case
      TextToken value _ -> Just value
      _ -> Nothing
    -- }}
    withTag CloseBlockToken
    return $ TextBlock value

expressionBlock :: Parser Block
expressionBlock = labeled "ExpressionBlock" $
  withPosition do
    -- {{
    withTag ExpressionBlockToken
    -- ...1 + 2...
    e <- expression
    -- }}
    closeBlock
    return $ ExpressionBlock e

chromeBlock :: Parser Block
chromeBlock = labeled "ChromeBlock" $
  withPosition do
    -- {{@
    withTag ChromeBlockToken
    -- ...layout "body"...
    e <- expression
    -- }}
    closeBlock
    -- {{...}}+
    bs <- many block
    return $ ChromeBlock e bs

altBlock :: Parser Block
altBlock = labeled "AltBlock" $ withPosition do
  -- {{#if doThis}} <- one of these
  startAlt <- labeled "StartAlt" $ withPosition do
    e <- between altToken closeBlock expression
    bs <- manyTill block closeAlt
    return $ ApplyBlock e bs
  -- {{#else doThat}} <- zero or more of these
  elseAlts <- flip manyTill closeElses $
    labeled "ElseAlt" $ withPosition do
      e <- between (altToken *> elseToken) closeBlock expression
      bs <- manyTill block closeAlt
      return $ ApplyBlock e bs
  -- {{#else}} <- zero or one of these
  defaultAlt <- optionMaybe . try $
    labeled "DefaultAlt" $ withPosition do
      defaultBlock
      bs <- manyTill block closeDefault
      return $ DefaultBlock bs
  -- {{#end}}
  endBlock
  return $ AltBlock (startAlt :| elseAlts) defaultAlt
  where
    closeAlt = lookAhead $ tryOne [endBlock, defaultBlock, elseBlock]
    closeElses = lookAhead $ tryOne [endBlock, defaultBlock]
    closeDefault = lookAhead $ try endBlock
    --
    endBlock = altToken *> endToken *> closeBlock
    defaultBlock = altToken *> elseToken *> closeBlock
    elseBlock = altToken *> elseToken
    --
    altToken = withTag AltBlockToken
    elseToken = withTag ElseToken
    endToken = withTag EndToken

textBlock :: Parser Block
textBlock = labeled "TextBlock" $
  withToken \case
    TextToken x pos -> Just $ TextBlock x pos
    _ -> Nothing

expression :: Parser Expression
expression = filterExpression <?> "Expression"
{-# INLINE expression #-}

filterExpression :: Parser Expression
filterExpression = applyExpression `chainl1` filtered <?> "FilterExpression"
  where
    filtered = withPosition do
      withTag PipeToken
      return f
    f pos x y = FilterExpression x y pos

applyExpression :: Parser Expression
applyExpression = chain . NEL.fromList <$> many1 accessExpression <?> "ApplyExpression"
  where
    chain (fn :| args) = chain' fn args
    chain' fn (arg : rest) = chain' (ApplyExpression fn arg (getExpressionPos arg)) rest
    chain' done [] = done

accessExpression :: Parser Expression
accessExpression = simpleExpression `chainl1` try accessed <?> "AccessExpression"
  where
    accessed = withPosition do
      withTag DotToken
      return f
    f pos x (NameExpression id' pos') = f pos x (StringExpression id' pos')
    f pos x y = AccessExpression x y pos

simpleExpression :: Parser Expression
simpleExpression =
  tryOne
    [ stringExpression,
      intExpression,
      doubleExpression,
      boolExpression,
      nameExpression,
      parensExpression,
      contextExpression,
      listExpression
    ]

stringExpression :: Parser Expression
stringExpression = labeled "StringLiteral" $
  withToken \case
    StringToken value pos -> Just (StringExpression value pos)
    _ -> Nothing

intExpression :: Parser Expression
intExpression = labeled "IntLiteral" $
  withToken \case
    IntToken value pos -> Just (IntExpression value pos)
    _ -> Nothing

doubleExpression :: Parser Expression
doubleExpression = labeled "DoubleLiteral" $
  withToken \case
    DoubleToken value pos -> Just (DoubleExpression value pos)
    _ -> Nothing

boolExpression :: Parser Expression
boolExpression = labeled "BoolLiteral" $
  withToken \case
    BoolToken value pos -> Just (BoolExpression value pos)
    _ -> Nothing

nameExpression :: Parser Expression
nameExpression = labeled "Name" $ withPosition do
  n <- NameExpression <$> withName
  try . notFollowedBy $ withTag ColonToken
  return n

parensExpression :: Parser Expression
parensExpression = do
  withTag OpenParenToken
  expression' <- expression
  withTag CloseParenToken
  return expression'

contextExpression :: Parser Expression
contextExpression =
  (braced <?> "BracedContext")
    <|> (unbraced <?> "UnbracedContext")
  where
    braced = withPosition do
      withTag OpenBraceToken
      pairs <- contextKeyValue `sepEndBy` comma
      withTag CloseBraceToken
      return $ ContextExpression pairs
    unbraced = withPosition do
      pairs <- contextKeyValue `sepBy1` comma
      return $ ContextExpression pairs

contextKeyValue :: Parser (String, Expression)
contextKeyValue = labeled "ContextLiteralPair" do
  key <- withName
  withTag ColonToken
  value <- expression
  return (key, value)

listExpression :: Parser Expression
listExpression = labeled "ListLiteral" do
  withPosition do
    withTag OpenBracketToken
    values <- expression `sepEndBy` comma
    withTag CloseBracketToken
    return $ ListExpression values

comma :: Parser ()
comma =
  labeled "Comma" $
    withTag CommaToken

closeBlock :: Parser ()
closeBlock =
  labeled "CloseBlock" $
    withTag CloseBlockToken

withToken :: (Token -> Maybe a) -> Parser a
withToken = P.token show getTokenPos

withTag :: TokenTag -> Parser ()
withTag tag = P.token show getTokenPos f
  where
    f = \case
      TaggedToken t _ | t == tag -> Just ()
      _ -> Nothing

withName :: Parser String
withName = withToken \case
  NameToken n _ -> Just n
  _ -> Nothing
