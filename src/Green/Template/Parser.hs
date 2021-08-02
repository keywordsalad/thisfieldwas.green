module Green.Template.Parser where

import Control.Monad ((>=>))
import Data.Functor
import Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Scientific
import Green.Template.Ast
import Text.Parsec hiding (token)
import qualified Text.Parsec as P
import Text.Parsec.Pos

{-
{{! This is a comment, it won't be rendered }}

{{! This expression is interpolated in-place: }}
{{ "this expression" }}

{{! This expression is piped through a filter: }}
{{ published | formateDate "%Y-%m-%d" }}

{{! Piping is just a fancy way of applying arguments forward: }}
{{ formateDate "%Y-%m-%d" published }}

{{! File inclusion is just a function call }}
{{ include "this/file.md" }}

{{! Contexts may be passed as arguments }}
{{ imageWith { title: "lolcat", src: "kitten.jpg" } }}
{{ "this/file.md" | includeWith { with: this, context: stuff } }}

{{# thisExpression.runs | using thisBody | as an argument }}
  This content is passed as a template to the function
  resolved by the above expression. The function may
  do with the template as it pleases, producing zero of
  more instances of the template output.

  {{# if expression }}
    An if-statement is simply a function that renders the
    body if the expression is truthy.
  {{# else }}
    The else-case is optional, and will be shown if the
    expression above is falsy.
  {{# end }}

  {{# each expression }}
    An each-statement is simply a function that renders the
    body for each item in the expression, using it as the
    context for the body.
  {{# else }}
    The else-case is optional, and will be shown if there
    are no items in the expression above.
  {{# end }}

  {{# if expression }}
    Sometimes you want more than one case...
  {{# else each expression }}
    For example, this will show if the above expression is
    falsy and this expression has items.
  {{# else }}
    And this will be shown if the above two cases are falsy.
    Any number of cases may be chained together, however feels
    appropriate. The keywords `else` and `end` are reserved
    here, but everything else is free to use!
  {{# end }}
{{# end }}
-}

type Lexer = Parsec String () (Token, SourcePos)

type Parser a = Parsec [(Token, SourcePos)] () a

type TemplateParser a = Parsec [Block] () a

data Token
  = OpenBlockToken -- "{{"
  | CloseBlockToken -- "}}"
  | OpenLayoutToken -- "@"
  | OpenTemplateToken -- "#"
  | OpenCommentToken -- "!"
  | OpenBraceToken -- "{"
  | CloseBraceToken -- "}"
  | OpenParenToken -- '('
  | CloseParenToken -- ')'
  | PipeToken -- "|"
  | CommaToken -- ","
  | DotToken -- "."
  | ColonToken -- ":"
  | EndToken -- "end"
  | ElseToken -- "else"
  | BoolToken Bool -- "true" | "false"
  | NameToken String -- ([_a-z])([_\-a-zA-Z0-9])*
  | StringToken String -- '"' [^"]* '"'
  | IntToken Int -- 0|[1-9][0-9]*
  | DoubleToken Double -- 0|[1-9][0-9]*\.[0-9]+
  | TextToken String
  deriving stock (Eq, Show)

parseTemplate :: String -> String -> Either ParseError Template
parseTemplate origin =
  parseTokens origin
    >=> parseBlocks origin
    >=> fmap intoTemplate . parseStructure origin
  where
    intoTemplate blocks = Template blocks (initialPos origin)

parseStructure :: String -> [Block] -> Either ParseError [Block]
parseStructure = parse (many structure <* eof)

parseBlocks :: String -> [(Token, SourcePos)] -> Either ParseError [Block]
parseBlocks = parse (many block <* eof)

parseTokens :: String -> String -> Either ParseError [(Token, SourcePos)]
parseTokens = parse (many token <* eof)

structure :: TemplateParser Block
structure =
  tryOne
    [ appliedLayout,
      appliedTemplate,
      withBlock \case
        b@ExpressionBlock {} -> Just b
        b@CommentBlock {} -> Just b
        b@TextBlock {} -> Just b
        _ -> Nothing
    ]

appliedLayout :: TemplateParser Block
appliedLayout = do
  (expression', pos) <- withBlock \case
    LayoutBlock expression' pos -> Just (expression', pos)
    _ -> Nothing
  structure' <- Template <$> many structure <*> pure pos
  return $ LayoutApplyBlock expression' structure' pos

appliedTemplate :: TemplateParser Block
appliedTemplate = do
  templates' <- (:|) <$> startTemplate <*> many nextTemplate
  elseTemplate' <- elseTemplate
  endBlock
  return $
    TemplateBlock
      templates'
      elseTemplate'
      (getTemplateApplyPos (NEL.head templates'))

startTemplate :: TemplateParser TemplateApplyBlock
startTemplate = do
  uncurry applyTemplate =<< withBlock \case
    TemplateStartBlock expression' pos -> Just (expression', pos)
    _ -> Nothing

nextTemplate :: TemplateParser TemplateApplyBlock
nextTemplate = do
  uncurry applyTemplate =<< withBlock \case
    TemplateNextBlock expression' pos -> Just (expression', pos)
    _ -> Nothing

applyTemplate :: Expression -> SourcePos -> TemplateParser TemplateApplyBlock
applyTemplate expression' pos = do
  structure' <- Template <$> manyTill structure (try nonStartBlock) <*> pure pos
  return $ TemplateApplyBlock expression' structure' pos

elseTemplate :: TemplateParser TemplateDefaultBlock
elseTemplate = do
  pos <- withBlock \case
    TemplateElseBlock pos -> Just pos
    _ -> Nothing
  structure' <- Template <$> manyTill structure (try endBlock) <*> pure pos
  return $ TemplateDefaultBlock structure' pos

nonStartBlock :: TemplateParser ()
nonStartBlock = withBlock \case
  TemplateNextBlock {} -> Just ()
  TemplateElseBlock {} -> Just ()
  TemplateEndBlock {} -> Just ()
  _ -> Nothing

endBlock :: TemplateParser ()
endBlock = withBlock \case
  TemplateEndBlock {} -> Just ()
  _ -> Nothing

withBlock :: (Block -> Maybe a) -> Parsec [Block] u a
withBlock = P.token showToken tokenPos
  where
    showToken = show
    tokenPos = getBlockPos

block :: Parser Block
block = tryOne [templateBlock, textBlock]

templateBlock :: Parser Block
templateBlock = do
  (_, position) <- eq OpenBlockToken
  f <-
    tryOne
      [ layoutBlock',
        commentBlock',
        templateBlock',
        interpolatedBlock'
      ]
  eq_ CloseBlockToken
  return $ f position
  where
    commentBlock' = do
      CommentBlock . fst <$> requireText

    interpolatedBlock' = do
      ExpressionBlock <$> expression

    layoutBlock' = do
      eq_ OpenLayoutToken
      LayoutBlock <$> expression

    templateBlock' = do
      eq_ OpenTemplateToken
      tryOne
        [ templateEndBlock',
          templateElseBlock',
          templateStartBlock'
        ]

    templateEndBlock' = do
      eq_ EndToken
      return TemplateEndBlock

    templateElseBlock' = do
      eq_ ElseToken
      default' <|> else'
      where
        default' = TemplateElseBlock <$ lookAhead (try (eq_ CloseBlockToken))
        else' = TemplateNextBlock <$> expression

    templateStartBlock' = do
      TemplateStartBlock <$> expression

textBlock :: Parser Block
textBlock = uncurry TextBlock <$> requireText

expression :: Parser Expression
expression = filterExpression
{-# INLINE expression #-}

filterExpression :: Parser Expression
filterExpression = applyExpression `chainl1` filtered
  where
    filtered = do
      (_, position) <- eq PipeToken
      return $ f position
    f position x y = FilterExpression x y position

applyExpression :: Parser Expression
applyExpression = chain . NEL.fromList <$> many1 accessExpression
  where
    chain (fn :| args) = chain' fn args
    chain' fn (arg : rest) = chain' (ApplyExpression fn arg (getExpressionPos arg)) rest
    chain' done [] = done

accessExpression :: Parser Expression
accessExpression = simpleExpression `chainl1` accessed
  where
    accessed = do
      (_, position) <- eq DotToken
      return $ f position
    f position x y = AccessExpression x y position

simpleExpression :: Parser Expression
simpleExpression =
  tryOne
    [ stringExpression,
      intExpression,
      doubleExpression,
      boolExpression,
      nameExpression,
      parensExpression,
      contextExpression
    ]

stringExpression :: Parser Expression
stringExpression = withToken \case
  (StringToken value, p) -> Just (StringExpression value p)
  _ -> Nothing

intExpression :: Parser Expression
intExpression = withToken \case
  (IntToken value, p) -> Just (IntExpression value p)
  _ -> Nothing

doubleExpression :: Parser Expression
doubleExpression = withToken \case
  (DoubleToken value, p) -> Just (DoubleExpression value p)
  _ -> Nothing

boolExpression :: Parser Expression
boolExpression = withToken \case
  (BoolToken value, p) -> Just (BoolExpression value p)
  _ -> Nothing

nameExpression :: Parser Expression
nameExpression = withToken \case
  (NameToken value, p) -> Just (NameExpression value p)
  _ -> Nothing

parensExpression :: Parser Expression
parensExpression = do
  eq_ OpenParenToken
  expression' <- expression
  eq_ CloseParenToken
  return expression'

contextExpression :: Parser Expression
contextExpression = do
  (_, position) <- eq OpenBraceToken
  pairs <- contextPair `sepBy` eq_ CommaToken
  eq_ CloseBraceToken
  return $ ContextExpression pairs position

contextPair :: Parser (String, Expression)
contextPair = do
  key <- withToken \case
    (NameToken value, _) -> Just value
    _ -> Nothing
  eq_ ColonToken
  value <- expression
  return (key, value)

eq :: Token -> Parser (Token, SourcePos)
eq t = expect (== t)

eq_ :: Token -> Parser ()
eq_ t = expect_ (== t)

expect :: (Token -> Bool) -> Parser (Token, SourcePos)
expect f = withToken \case
  tokenPos | f (fst tokenPos) -> Just tokenPos
  _ -> Nothing

expect_ :: (Token -> Bool) -> Parser ()
expect_ f = () <$ expect f

requireText :: Parser (String, SourcePos)
requireText = withToken f
  where
    f = \case
      (TextToken text, position) -> Just (text, position)
      _ -> Nothing

withToken :: ((Token, SourcePos) -> Maybe a) -> Parsec [(Token, SourcePos)] u a
withToken = P.token showToken tokenPos
  where
    showToken = show . fst
    tokenPos = snd

withPosition :: Parsec String () Token -> Lexer
withPosition p = do
  position <- P.getPosition
  token' <- p
  return (token', position)

token :: Lexer
token =
  tryOne
    [ symbolToken,
      numberToken,
      boolToken,
      endKeyword,
      elseKeyword,
      nameToken,
      stringToken,
      textToken
    ]

textToken :: Lexer
textToken =
  withPosition (TextToken <$> textToken') <?> "textToken"
  where
    textToken' = mconcat <$> many1 (many1 chars <|> braceEscape)
    chars =
      tryOne
        [ textChar,
          openBraceChar,
          closeBraceChar,
          justBackslash,
          braceChar
        ]
    textChar = noneOf "{}\\"
    justBackslash = char '\\' <* notFollowedBy braceChar
    braceEscape = do
      _ <- char '\\'
      string "{{" <|> string "}}"

symbolToken :: Lexer
symbolToken =
  withPosition $
    tryOne
      [ ws (OpenBraceToken <$ openBraceChar <?> "OpenBrace '{'"),
        ws (CloseBraceToken <$ closeBraceChar <?> "CloseBrace '}'"),
        ws (OpenParenToken <$ char '(' <?> "OpenParen '('"),
        ws (CloseParenToken <$ char ')' <?> "CloseParen ')'"),
        ws (OpenBlockToken <$ string "{{" <?> "OpenBlock '{{'"),
        CloseBlockToken <$ string "}}" <?> "CloseBlock '}}'",
        ws (OpenTemplateToken <$ char '#' <?> "TemplateBlock '#'"),
        ws (OpenLayoutToken <$ char '@' <?> "LayoutBlock '@'"),
        OpenCommentToken <$ char '!' <?> "CommentBlock '!'",
        ws (PipeToken <$ char '|' <?> "Pipe '|'"),
        ws (ColonToken <$ char ':' <?> "Colon ':'"),
        ws (DotToken <$ char '.' <?> "Dot '.'"),
        ws (CommaToken <$ char ',' <?> "Comma ','")
      ]

boolToken :: Lexer
boolToken = withPosition (BoolToken <$> value <?> "Bool")
  where
    value =
      True <$ keyword "true"
        <|> False <$ keyword "false"

stringToken :: Lexer
stringToken = withPosition $ ws (StringToken <$> stringChars <?> "String")
  where
    stringChars = between dquote dquote (many (stringChar <|> escapeChar))
    dquote = char '"'
    stringChar = noneOf "\\\"\n"
    escapeChar =
      char '\\'
        *> tryOne
          [ char '\\',
            char '\'',
            char '"',
            char 'n' $> '\n',
            char 't' $> '\t'
          ]

numberToken :: Lexer
numberToken = withPosition $ ws (token' <?> "Number")
  where
    token' = convert <$> (ints <> option "" doubles) <* notFollowedBy badChars
    ints = (string "0" <* notFollowedBy nonZero) <|> ((:) <$> nonZero <*> many digit)
    doubles = string "." <> many1 digit
    nonZero = oneOf ['1' .. '9']
    badChars = oneOf "_." <|> alphaNum
    convert s =
      case (read s :: Scientific) of
        n | isInteger n -> IntToken (fromJust (toBoundedInteger n))
        n | otherwise -> DoubleToken (toRealFloat n)

nameToken :: Lexer
nameToken = withPosition $ ws (NameToken <$> name <?> "Name")

braceChar :: Parsec String () Char
braceChar = openBraceChar <|> closeBraceChar

openBraceChar :: Parsec String () Char
openBraceChar = char '{' <* notFollowedBy (char '{')

closeBraceChar :: Parsec String () Char
closeBraceChar = char '}' <* notFollowedBy (char '}')

endKeyword :: Lexer
endKeyword = withPosition (EndToken <$ keyword "end")

elseKeyword :: Lexer
elseKeyword = withPosition (ElseToken <$ keyword "else")

keyword :: String -> Parsec String u ()
keyword s = ws p
  where
    p = do
      _ <- string s <?> "Keyword '" ++ s ++ "'"
      notFollowedBy nameRest
      return ()

name :: Parsec String u String
name = ws do
  start <- nameStart
  rest <- many nameRest
  return (start : rest)

nameStart :: Parsec String u Char
nameStart = oneOf ('_' : ['a' .. 'z'])

nameRest :: Parsec String u Char
nameRest = tryOne [nameStart, alphaNum, char '-']

ws :: Parsec String u a -> Parsec String u a
ws = (<* spaces)

tryOne :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
tryOne = choice . fmap try
