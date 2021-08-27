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
lex = runParser $ many token <* eof

token :: Lexer Token
token =
  getLexerMode >>= \case
    BlockMode ->
      blockToken >>= \case
        t@TurnOffToken {} -> do
          startFenced "{{*"
          return t
        --
        t@CommentBlockToken {} -> do
          startFenced "{{!"
          return t
        --
        t@CloseBlockToken {} -> do
          startText
          return t
        --
        t -> do
          return t
    --
    TextMode ->
      tryOne
        [ do
            _ <- lookAhead $ try (string "{{")
            startBlock
            token,
          do
            t <- textToken
            startBlock
            return t
        ]
    FencedMode _ open -> fencedText open

startText :: (Show t, Stream s m t) => ParsecT s ParserState m ()
startText = putLexerMode TextMode <?> "StartText"

startBlock :: (Show t, Stream s m t) => ParsecT s ParserState m ()
startBlock = putLexerMode BlockMode <?> "StartBlock"

startFenced :: (Show t, Stream s m t) => String -> ParsecT s ParserState m ()
startFenced open = putLexerMode (FencedMode 0 open) <?> "StartFenced"

illegalState :: (Show t, Stream s m t) => ParsecT s ParserState m a
illegalState = p <?> "IllegalState"
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
textToken = withPosition (TextToken <$> text) <?> "TextToken"

fencedText :: String -> Lexer Token
fencedText openStr = withPosition (TextToken <$> p "") <?> "FencedText"
  where
    open = string openStr
    close = string "}}"
    p acc =
      tryOne
        [ downFence acc,
          upFence acc,
          fenceText acc
        ]
    -- {{* / {{!
    downFence acc = labeled "DownFence" do
      x <- tryOne [open, string "{{"]
      downFenceLevel
      p (acc ++ x)
    -- }}
    upFence acc = labeled "UpFence" do
      _ <- lookAhead $ try close
      fenceLevel >>= \case
        0 -> do
          startBlock
          return acc
        _ -> do
          x <- close
          upFenceLevel
          p (acc ++ x)
    -- ...abc...
    fenceText acc = labeled "FenceText" do
      t <- text
      p (acc ++ t)
    --
    downFenceLevel = do
      n <- fenceLevel
      putLexerMode $ FencedMode (n + 1) openStr
    upFenceLevel = do
      n <- fenceLevel
      putLexerMode $ FencedMode (n - 1) openStr
    fenceLevel =
      getLexerMode >>= \case
        FencedMode n _ -> return n
        _ -> illegalState

text :: Lexer String
text = p <?> "TextBody"
  where
    p = mconcat <$> many1 (tryOne [blockEscape, chars])
    chars =
      many1 $
        tryOne
          [ textChar,
            justBackslash,
            justBrace
          ]
    textChar = noneOf "{}\\" <?> "CommentChar"
    justBackslash = char '\\' <* notFollowedBy (try (oneOf "{}")) <?> "Backslash '\\'"
    blockEscape = char '\\' *> (string "{{" <|> string "}}") <?> "BlockEscape"
    justBrace = tryOne [justOpenBrace, justCloseBrace] <?> "Brace"
      where
        justOpenBrace = char '{' <* notFollowedBy (try (char '{'))
        justCloseBrace = char '}' <* notFollowedBy (try (char '}'))

symbolToken :: Lexer Token
symbolToken =
  withPosition $
    tryOne
      [ mkSymbol TurnOffToken "{{*",
        mkSymbol CommentBlockToken "{{!",
        mkSymbol IncludeBlockToken "{{>" <* spaces,
        mkSymbol AltBlockToken "{{#" <* spaces,
        mkSymbol ChromeBlockToken "{{@" <* spaces,
        mkSymbol ExpressionBlockToken "{{" <* spaces,
        mkSymbol CloseBlockToken "}}",
        mkSymbol OpenBraceToken "{" <* spaces,
        mkSymbol CloseBraceToken "}" <* spaces,
        mkSymbol OpenParenToken "(" <* spaces,
        mkSymbol CloseParenToken ")" <* spaces,
        mkSymbol OpenBracketToken "[" <* spaces,
        mkSymbol CloseBracketToken "]" <* spaces,
        mkSymbol PipeToken "|" <* spaces,
        mkSymbol ColonToken ":" <* spaces,
        mkSymbol DotToken "." <* spaces,
        mkSymbol CommaToken "," <* spaces
      ]
  where
    mkSymbol t s = t <$ string s <?> "Token " ++ show s

boolToken :: Lexer Token
boolToken = withPosition (BoolToken <$> value) <?> "BoolToken"
  where
    value =
      True <$ (keyword "True" <|> keyword "true")
        <|> False <$ (keyword "False" <|> keyword "false")

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
nameToken = withPosition $ (NameToken <$> name <?> "NameToken") <* spaces

endKeyword :: Lexer Token
endKeyword = withPosition (EndToken <$ keyword "end")

elseKeyword :: Lexer Token
elseKeyword = withPosition (ElseToken <$ keyword "else")

keyword :: String -> Lexer ()
keyword s = p <* spaces
  where
    p = do
      _ <- string s <?> "Keyword '" ++ s ++ "'"
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
nameRest = tryOne [nameStart, alphaNum, char '-'] <?> "NameRest"

data Token
  = ExpressionBlockToken SourcePos -- "{{"
  | CommentBlockToken SourcePos -- "{{!"
  | IncludeBlockToken SourcePos -- "{{>"
  | AltBlockToken SourcePos -- "{{#"
  | ChromeBlockToken SourcePos -- "{{@"
  | CloseBlockToken SourcePos -- "}}"
  | OpenParenToken SourcePos -- "("
  | CloseParenToken SourcePos -- ")"
  | OpenBracketToken SourcePos -- "["
  | CloseBracketToken SourcePos -- "]"
  | OpenBraceToken SourcePos -- "{"
  | CloseBraceToken SourcePos -- "}"
  | PipeToken SourcePos -- "|"
  | CommaToken SourcePos -- ","
  | DotToken SourcePos -- "."
  | ColonToken SourcePos -- ":"
  | EndToken SourcePos -- "end"
  | ElseToken SourcePos -- "else"
  | BoolToken Bool SourcePos -- "true" | "false"
  | NameToken String SourcePos -- ([_a-z])([_\-a-zA-Z0-9])*
  | StringToken String SourcePos -- text enclosed by "" or ''
  | IntToken Int SourcePos -- 0|[1-9][0-9]*
  | DoubleToken Double SourcePos -- 0|[1-9][0-9]*\.[0-9]+
  | TextToken String SourcePos
  | TurnOffToken SourcePos -- "{{*
  deriving stock (Eq, Show)

getTokenPos :: Token -> SourcePos
getTokenPos = \case
  ExpressionBlockToken pos -> pos
  CommentBlockToken pos -> pos
  IncludeBlockToken pos -> pos
  AltBlockToken pos -> pos
  ChromeBlockToken pos -> pos
  CloseBlockToken pos -> pos
  OpenParenToken pos -> pos
  CloseParenToken pos -> pos
  OpenBracketToken pos -> pos
  CloseBracketToken pos -> pos
  OpenBraceToken pos -> pos
  CloseBraceToken pos -> pos
  PipeToken pos -> pos
  CommaToken pos -> pos
  DotToken pos -> pos
  ColonToken pos -> pos
  EndToken pos -> pos
  ElseToken pos -> pos
  BoolToken _ pos -> pos
  NameToken _ pos -> pos
  StringToken _ pos -> pos
  IntToken _ pos -> pos
  DoubleToken _ pos -> pos
  TextToken _ pos -> pos
  TurnOffToken pos -> pos
