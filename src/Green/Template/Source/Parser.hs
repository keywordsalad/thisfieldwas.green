module Green.Template.Source.Parser where

import Data.Functor
import Data.List.NonEmpty as NEL
import Data.Maybe
import Data.String
import Green.Template.Ast
import Green.Template.Source.Lexer
import Green.Template.Source.Util
import Text.Parsec hiding (runParser, token, tokens, (<?>))
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
    withToken \case
      CommentBlockToken {} -> Just ()
      _ -> Nothing
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
    withToken \case
      TurnOffToken {} -> Just ()
      _ -> Nothing
    -- ...text...
    value <- withToken \case
      TextToken value _ -> Just value
      _ -> Nothing
    -- }}
    withToken \case
      CloseBlockToken {} -> Just ()
      _ -> Nothing
    return $ TextBlock value

expressionBlock :: Parser Block
expressionBlock = labeled "ExpressionBlock" $
  withPosition do
    -- {{
    withToken \case
      ExpressionBlockToken {} -> Just ()
      _ -> Nothing
    -- ...1 + 2...
    e <- expression
    -- }}
    closeBlock
    return $ ExpressionBlock e

chromeBlock :: Parser Block
chromeBlock = labeled "ChromeBlock" $
  withPosition do
    -- {{@
    withToken \case
      ChromeBlockToken {} -> Just ()
      _ -> Nothing
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
  elseAlts <- flip manyTill closeElses $
    labeled "ElseAlt" $ withPosition do
      e <- between (altToken *> elseToken) closeBlock expression
      bs <- manyTill block closeAlt
      return $ ApplyBlock e bs
  defaultAlt <- optionMaybe . try $
    labeled "DefaultAlt" $ withPosition do
      defaultBlock
      bs <- manyTill block closeDefault
      return $ DefaultBlock bs
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
    altToken = withToken \case
      AltBlockToken {} -> Just ()
      _ -> Nothing
    elseToken = withToken \case
      ElseToken {} -> Just ()
      _ -> Nothing
    endToken = withToken \case
      EndToken {} -> Just ()
      _ -> Nothing

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
    filtered =
      f <$> withToken \case
        PipeToken pos -> Just pos
        _ -> Nothing
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
    accessed =
      f <$> withToken \case
        DotToken pos -> Just pos
        _ -> Nothing
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
nameExpression = labeled "Name" do
  name' <- withToken \case
    NameToken value pos -> Just (NameExpression value pos)
    _ -> Nothing
  try $
    notFollowedBy $ withToken \case
      ColonToken {} -> return ()
      t -> fail $ "Received " ++ show t
  return name'

parensExpression :: Parser Expression
parensExpression = do
  withToken \case
    OpenParenToken {} -> Just ()
    _ -> Nothing
  expression' <- expression
  withToken \case
    CloseParenToken {} -> Just ()
    _ -> Nothing
  return expression'

contextExpression :: Parser Expression
contextExpression =
  (braced <?> "BracedContextLiteral")
    <|> (open <?> "OpenContextLiteral")
  where
    braced = withPosition do
      withToken \case
        OpenBraceToken {} -> Just ()
        _ -> Nothing
      pairs <- contextKeyValue `sepEndBy` comma
      withToken \case
        CloseBraceToken {} -> Just ()
        _ -> Nothing
      return $ ContextExpression pairs
    open = withPosition do
      pairs <- contextKeyValue `sepBy1` comma
      return $ ContextExpression pairs

contextKeyValue :: Parser (String, Expression)
contextKeyValue = labeled "ContextLiteralPair" do
  key <- withToken \case
    NameToken key _ -> Just key
    _ -> Nothing
  withToken \case
    ColonToken {} -> Just ()
    _ -> Nothing
  value <- expression
  return (key, value)

listExpression :: Parser Expression
listExpression = labeled "ListLiteral" do
  withPosition do
    withToken \case
      OpenBracketToken {} -> Just ()
      _ -> Nothing
    values <- expression `sepEndBy` comma
    withToken \case
      CloseBracketToken {} -> Just ()
      _ -> Nothing
    return $ ListExpression values

comma :: Parser ()
comma = labeled "Comma" $
  withToken \case
    CommaToken {} -> Just ()
    _ -> Nothing

closeBlock :: Parser ()
closeBlock = labeled "CloseBlock" $
  withToken \case
    CloseBlockToken {} -> Just ()
    _ -> Nothing

withToken :: (Token -> Maybe a) -> Parser a
withToken = P.token show getTokenPos
