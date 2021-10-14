module Green.Template.Source.Lexer where

import Data.Functor
import Data.Maybe
import Data.Scientific
import Data.String
import Green.Template.Source.Util
import Text.Parsec hiding (runParser, token, tokens, (<?>))
import Prelude hiding (lex)

type Lexer a = Parsec String ParserState a

lex :: SourceName -> String -> Either ParseError [Token]
lex = runParser $ manyTill token eof

token :: Lexer Token
token =
  getLexerMode >>= \case
    --
    BlockMode ->
      blockToken >>= \t -> case t of
        TaggedToken TurnOffToken _ -> t <$ startFenced
        TaggedToken CommentBlockToken _ -> t <$ startFenced
        TaggedToken CloseBlockToken _ -> t <$ startText
        _ -> return t
    --
    TextMode ->
      tryOne
        [ do
            _ <- lookAhead $ try (string "{{")
            startBlock
            token,
          textToken
        ]
    --
    FencedMode {} -> fencedText

startText :: (Show t, Stream s m t) => ParsecT s ParserState m ()
startText = putLexerMode TextMode <?> "starting text"

startBlock :: (Show t, Stream s m t) => ParsecT s ParserState m ()
startBlock = putLexerMode BlockMode <?> "starting block"

startFenced :: (Show t, Stream s m t) => ParsecT s ParserState m ()
startFenced = putLexerMode (FencedMode 1) <?> "starting fenced text"

illegalState :: (Show t, Stream s m t) => ParsecT s ParserState m a
illegalState = p <?> "illegal state"
  where
    p = do
      mode <- getLexerMode
      fail $ "Illegal lexer mode: " ++ show mode

blockToken :: Lexer Token
blockToken =
  tryOne
    [ symbolToken,
      numberToken,
      boolToken,
      endKeyword,
      elseKeyword,
      nameToken,
      stringToken
    ]

textToken :: Lexer Token
textToken = withPosition (TextToken <$> text) <?> "text"

fencedText :: Lexer Token
fencedText = withPosition (TextToken <$> p "")
  where
    p acc =
      tryOne
        [ downFence acc,
          upFence acc,
          fenceText acc
        ]
    -- {{
    downFence acc = do
      x <- string "{{"
      downFenceLevel
      p (acc ++ x)
    -- }}
    upFence acc = do
      lookAhead (try $ string "}}") *> upFenceLevel >>= \case
        0 -> do
          startBlock
          return acc
        _ -> do
          x <- string "}}"
          p (acc ++ x)
    -- ...abc...
    fenceText acc = do
      t <- text
      p (acc ++ t)
    --
    downFenceLevel = do
      n <- succ <$> fenceLevel
      putLexerMode $ FencedMode n
    upFenceLevel = do
      n <- pred <$> fenceLevel
      putLexerMode $ FencedMode n
      return n
    fenceLevel =
      getLexerMode >>= \case
        FencedMode n -> return n
        _ -> illegalState

text :: Lexer String
text = do
  cs <- manyTill text' (lookAhead $ try textTerminator)
  return $ mconcat cs
  where
    text' =
      tryOne
        [ justBrace,
          justBackslash,
          blockEscape,
          spaceString,
          textString
        ]
    justBrace = tryOne [justOpenBrace, justCloseBrace]
      where
        justOpenBrace = openBrace' <* notFollowedBy (try openBrace') <?> "just '{'"
        justCloseBrace = closeBrace' <* notFollowedBy (try closeBrace') <?> "just '}'"
    justBackslash = string "\\" <* notFollowedBy (try $ oneOf "{}-") <?> "just '\\'"
    blockEscape = char '\\' *> tryOne [open', close', trimmingClose] <?> "escaped block"
    textString = many1 (noneOf "{}\\\n\t ") <?> "text string"
    spaceString = many1 space <* notFollowedBy (try $ string "{{-") <?> "space string"
    textTerminator =
      labeled "text terminator" $
        tryOne
          [ open',
            close',
            spaces *> string "{{-",
            "" <$ eof
          ]
    open' = string open
    close' = string close
    openBrace' = string "{"
    closeBrace' = string "}"

symbolToken :: Lexer Token
symbolToken =
  withPosition $
    tryOne
      [ mkTrimmingSymbol TurnOffToken,
        mkTrimmingSymbol CommentBlockToken,
        mkTrimmingSymbol IncludeBlockToken <* spaces,
        mkTrimmingSymbol AltBlockToken <* spaces,
        mkTrimmingSymbol ChromeBlockToken <* spaces,
        mkTrimmingSymbol ExpressionBlockToken <* spaces,
        mkTrimmingSymbol CloseBlockToken <* spaces,
        --
        mkSymbol TurnOffToken,
        mkSymbol CommentBlockToken,
        mkSymbol IncludeBlockToken <* spaces,
        mkSymbol AltBlockToken <* spaces,
        mkSymbol ChromeBlockToken <* spaces,
        mkSymbol ExpressionBlockToken <* spaces,
        mkSymbol CloseBlockToken,
        --
        mkSymbol OpenBraceToken <* spaces,
        mkSymbol CloseBraceToken <* spaces,
        mkSymbol OpenParenToken <* spaces,
        mkSymbol CloseParenToken <* spaces,
        mkSymbol OpenBracketToken <* spaces,
        mkSymbol CloseBracketToken <* spaces,
        mkSymbol PipeToken <* spaces,
        mkSymbol ColonToken <* spaces,
        mkSymbol DotToken <* spaces,
        mkSymbol CommaToken <* spaces
      ]
  where
    mkSymbol t = TaggedToken t <$ string (tokenTagValue t) <?> show t
    mkTrimmingSymbol t = TaggedToken t <$ string (trimmingTokenTagValue t) <* spaces <?> showTrimmingTokenTag t

boolToken :: Lexer Token
boolToken = withPosition (BoolToken <$> value) <?> "BoolToken"
  where
    value =
      True <$ tryOne [keyword "True", keyword "true"]
        <|> False <$ tryOne [keyword "False", keyword "false"]

stringToken :: Lexer Token
stringToken = labeled "StringToken" $ withPosition $ (StringToken <$> stringChars) <* spaces
  where
    stringChars =
      tryOne
        [ between squote squote $ many (noneOf "\\\'\n" <|> escapeChar),
          between dquote dquote $ many (noneOf "\\\"\n" <|> escapeChar)
        ]
    squote = char '\''
    dquote = char '"'
    escapeChar =
      labeled "EscapeChar" $
        char '\\'
          *> tryOne
            [ char '\\',
              char '\'',
              char '"',
              char 'n' $> '\n',
              char 't' $> '\t'
            ]

numberToken :: Lexer Token
numberToken = withPosition (numberToken' <* spaces)
  where
    numberToken' = do
      value <- ints <> option "" doubles
      notFollowedBy badChars
      case (read value :: Scientific) of
        n | isInteger n -> return (IntToken (fromJust (toBoundedInteger n))) <?> "IntToken"
        n | otherwise -> return (DoubleToken (toRealFloat n)) <?> "DoubleToken"

    ints = justZero <|> ((:) <$> nonZero <*> many digit) <?> "IntegerDigits"
    justZero = string "0" <* notFollowedBy nonZero <?> "JustZero"
    nonZero = oneOf ['1' .. '9'] <?> "NonZeroDigit"
    doubles = string "." <> many1 digit <?> "DoubleDigits"
    badChars = oneOf "_." <|> alphaNum

nameToken :: Lexer Token
nameToken =
  withPosition $
    (NameToken <$> name <?> "NameToken") <* spaces

endKeyword :: Lexer Token
endKeyword =
  withPosition $
    TaggedToken EndToken <$ keyword (tokenTagValue EndToken)

elseKeyword :: Lexer Token
elseKeyword =
  withPosition $
    TaggedToken ElseToken <$ keyword (tokenTagValue ElseToken)

keyword :: String -> Lexer ()
keyword s = p <* spaces
  where
    p = do
      _ <- string s <?> "KeywordToken " ++ show s
      notFollowedBy nameRest
      return ()

name :: Lexer String
name = p <?> "NameValue"
  where
    p = do
      start <- nameStart
      rest <- many nameRest
      spaces
      return (start : rest)

nameStart :: Lexer Char
nameStart = oneOf ('_' : ['a' .. 'z']) <?> "NameStart"

nameRest :: Lexer Char
nameRest = tryOne [nameStart, alphaNum] <?> "NameRest"

data Token
  = TaggedToken TokenTag SourcePos
  | BoolToken Bool SourcePos
  | NameToken String SourcePos
  | StringToken String SourcePos
  | IntToken Int SourcePos
  | DoubleToken Double SourcePos
  | TextToken String SourcePos
  deriving stock (Eq)

instance Show Token where
  show t = case t of
    TaggedToken tt _ -> show tt
    BoolToken b _ -> "Bool " ++ show b
    NameToken s _ -> "Name " ++ show s
    StringToken s _ -> "String " ++ show s
    IntToken n _ -> "Int " ++ show n
    DoubleToken x _ -> "Double " ++ show x
    TextToken s _ -> "Text " ++ show s

getTokenPos :: Token -> SourcePos
getTokenPos = \case
  TaggedToken _ pos -> pos
  BoolToken _ pos -> pos
  NameToken _ pos -> pos
  StringToken _ pos -> pos
  IntToken _ pos -> pos
  DoubleToken _ pos -> pos
  TextToken _ pos -> pos

data TokenTag
  = ExpressionBlockToken
  | CommentBlockToken
  | IncludeBlockToken
  | AltBlockToken
  | ChromeBlockToken
  | CloseBlockToken
  | OpenParenToken
  | CloseParenToken
  | OpenBraceToken
  | CloseBraceToken
  | OpenBracketToken
  | CloseBracketToken
  | PipeToken
  | CommaToken
  | DotToken
  | ColonToken
  | EndToken
  | ElseToken
  | TurnOffToken
  deriving stock (Eq)

tokenTagName :: TokenTag -> String
tokenTagName = \case
  ExpressionBlockToken -> "ExpressionBlock"
  CommentBlockToken -> "CommentBlock"
  IncludeBlockToken -> "IncludeBlock"
  AltBlockToken -> "AltBlock"
  ChromeBlockToken -> "ChromeBlock"
  CloseBlockToken -> "CloseBlock"
  OpenParenToken -> "OpenParen"
  CloseParenToken -> "CloseParen"
  OpenBracketToken -> "OpenBracket"
  CloseBracketToken -> "CloseBracket"
  OpenBraceToken -> "OpenBrace"
  CloseBraceToken -> "CloseBrace"
  PipeToken -> "PipeToken"
  CommaToken -> "CommaToken"
  DotToken -> "DotToken"
  ColonToken -> "ColonToken"
  EndToken -> "EndToken"
  ElseToken -> "ElseToken"
  TurnOffToken -> "TurnOff"

tokenTagValue :: TokenTag -> String
tokenTagValue = \case
  ExpressionBlockToken -> open
  CommentBlockToken -> "{{!"
  IncludeBlockToken -> "{{>"
  AltBlockToken -> "{{#"
  ChromeBlockToken -> "{{@"
  CloseBlockToken -> "}}"
  OpenParenToken -> "("
  CloseParenToken -> ")"
  OpenBraceToken -> "{"
  CloseBraceToken -> "}"
  OpenBracketToken -> "["
  CloseBracketToken -> "]"
  PipeToken -> "|"
  CommaToken -> ","
  DotToken -> "."
  ColonToken -> ":"
  EndToken -> "end"
  ElseToken -> "else"
  TurnOffToken -> "{{*"

tokenTagParser :: TokenTag -> Lexer String
tokenTagParser tag = string (tokenTagValue tag) <?> show tag

trimmingTokenTagValue :: TokenTag -> String
trimmingTokenTagValue = \case
  ExpressionBlockToken -> "{{-"
  CommentBlockToken -> "{{-!"
  IncludeBlockToken -> "{{->"
  AltBlockToken -> "{{-#"
  ChromeBlockToken -> "{{-@"
  CloseBlockToken -> "-}}"
  TurnOffToken -> "{{-*"
  t -> tokenTagValue t

trimmingTokenTagParser :: TokenTag -> Lexer String
trimmingTokenTagParser tag = string (trimmingTokenTagValue tag) <?> showTrimmingTokenTag tag

showTrimmingTokenTag :: TokenTag -> String
showTrimmingTokenTag t = tokenTagName t ++ " " ++ show (trimmingTokenTagValue t)

open :: String
open = "{{"

close :: String
close = "}}"

trimmingOpen :: Lexer String
trimmingOpen = trimmingTokenTagParser ExpressionBlockToken

trimmingClose :: Lexer String
trimmingClose = trimmingTokenTagParser CloseBlockToken

openBrace :: Lexer String
openBrace = tokenTagParser OpenBraceToken

closeBrace :: Lexer String
closeBrace = tokenTagParser CloseBraceToken

instance Show TokenTag where
  show t = tokenTagName t ++ " " ++ show (tokenTagValue t)
