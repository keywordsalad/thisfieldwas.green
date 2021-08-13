module Green.Template.Parser where

import Control.Monad.Identity
import Data.Default
import Data.Functor
import Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Scientific
import Green.Template.Ast
import Text.Parsec hiding (runParser, token, (<?>))
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
  {{ else }}
    The else-case is optional, and will be shown if the
    expression above is falsy.
  {{ end }}

  {{# each expression }}
    An each-statement is simply a function that renders the
    body for each item in the expression, using it as the
    context for the body.
  {{ else }}
    The else-case is optional, and will be shown if there
    are no items in the expression above.
  {{ end }}

  {{# if expression }}
    Sometimes you want more than one case...
  {{ else each expression }}
    For example, this will show if the above expression is
    falsy and this expression has items.
  {{ else }}
    And this will be shown if the above two cases are falsy.
    Any number of cases may be chained together, however feels
    appropriate. The keywords `else` and `end` are reserved
    here, but everything else is free to use!
  {{ end }}
{{ end }}
-}

data ParserState = ParserState
  { parserStateLexerMode :: LexerMode,
    parserStateIsDebugging :: Bool
  }

instance Default ParserState where
  def =
    ParserState
      { parserStateLexerMode = TextMode,
        parserStateIsDebugging = False
      }

data LexerMode = TextMode | BlockMode | CommentMode Int
  deriving stock (Show)

type BlockParser a = Parsec [Block] ParserState a

type TokenParser a = Parsec [Token] ParserState a

type Lexer a = Parsec String ParserState a

runParser :: (Stream s Identity t) => Parsec s ParserState a -> SourceName -> s -> Either ParseError a
runParser = runParserWith state
  where
    state = def

debugRunParser :: (Stream s Identity t) => Parsec s ParserState a -> SourceName -> s -> Either ParseError a
debugRunParser = runParserWith state
  where
    state = def {parserStateIsDebugging = True}

runParserWith :: (Stream s Identity t) => ParserState -> Parsec s ParserState a -> SourceName -> s -> Either ParseError a
runParserWith state p = P.runParser p state

parseTemplate :: SourceName -> String -> Either ParseError Template
parseTemplate origin =
  runParser (many token <* eof) origin
    >=> runParser (many block <* eof) origin
    >=> fmap intoTemplate . runParser (many structure <* eof) origin
  where
    intoTemplate blocks = Template blocks (initialPos origin)

structure :: BlockParser Block
structure =
  tryOne
    [ blockStructure,
      appliedLayout,
      appliedTemplate
    ]

blockStructure :: BlockParser Block
blockStructure = withBlock \b -> case b of
  TextBlock {} -> Just b
  CommentBlock {} -> Just b
  ExpressionBlock {} -> Just b
  _ -> Nothing

appliedLayout :: BlockParser Block
appliedLayout = p <?> "AppliedLayoutStructure"
  where
    p = withPosition do
      expression' <- withBlock \case
        LayoutBlock e _ -> Just e
        _ -> Nothing
      structure' <- Template <$> many structure
      return \pos -> LayoutApplyBlock expression' (structure' pos) pos

appliedTemplate :: BlockParser Block
appliedTemplate = p <?> "AppliedTemplateStructure"
  where
    p = withPosition do
      templates' <- (:|) <$> startTemplate <*> many (try nextTemplate)
      defaultTemplate' <- optionMaybe (try defaultTemplate)
      endBlock
      trace' "ALL DONE!"
      return $ TemplateBlock templates' defaultTemplate'

startTemplate :: BlockParser TemplateApplyBlock
startTemplate = p <?> "StartTemplateStructure"
  where
    p = withPosition do
      applyTemplate =<< withBlock \case
        TemplateStartBlock e _ -> Just e
        _ -> Nothing

nextTemplate :: BlockParser TemplateApplyBlock
nextTemplate = p <?> "NextTemplateStructure"
  where
    p = withPosition do
      applyTemplate =<< withBlock \case
        TemplateNextBlock e _ -> Just e
        _ -> Nothing

applyTemplate :: Expression -> BlockParser (SourcePos -> TemplateApplyBlock)
applyTemplate expression' = do
  structures' <- manyTill structure (lookAhead thisTemplateEnds)
  return \pos -> TemplateApplyBlock expression' (Template structures' pos) pos
  where
    thisTemplateEnds = try $ withBlock \case
      TemplateNextBlock {} -> Just ()
      TemplateElseBlock {} -> Just ()
      TemplateEndBlock {} -> Just ()
      _ -> Nothing

defaultTemplate :: BlockParser TemplateDefaultBlock
defaultTemplate = p <?> "DefaultTemplateStructure"
  where
    p = withPosition do
      trace' "GET BLOCK HERE"
      withBlock \case
        TemplateElseBlock {} -> Just ()
        _ -> Nothing
      trace' "GET STRUCTURES HERE"
      structure' <- Template <$> manyTill structure (lookAhead thisTemplateEnds)
      trace' "END THE TEMPLATE"
      return \pos -> TemplateDefaultBlock (structure' pos) pos
    thisTemplateEnds = try endBlock

endBlock :: BlockParser ()
endBlock = withBlock \case
  TemplateEndBlock {} -> Just ()
  _ -> Nothing

withBlock :: (Block -> Maybe a) -> Parsec [Block] u a
withBlock = P.token showToken tokenPos
  where
    showToken = show
    tokenPos = getBlockPos

block :: TokenParser Block
block = tryOne [templateBlock, textBlock]

templateBlock :: TokenParser Block
templateBlock = p <?> "TemplateBlock"
  where
    p =
      tryOne
        [ layoutBlock,
          commentBlock,
          templateEndBlock,
          templateDefaultBlock,
          templateNextBlock,
          templateStartBlock,
          expressionBlock
        ]

openBlock :: TokenParser ()
openBlock = withToken \case
  OpenBlockToken {} -> Just ()
  _ -> Nothing

closeBlock :: TokenParser ()
closeBlock = withToken \case
  CloseBlockToken {} -> Just ()
  _ -> Nothing

commentBlock :: TokenParser Block
commentBlock = p <?> "CommentBlock"
  where
    p = withPosition do
      openBlock
      withToken \case
        OpenCommentToken {} -> Just ()
        _ -> Nothing
      value <- withToken \case
        TextToken value _ -> Just value
        _ -> Nothing
      closeBlock
      return $ CommentBlock value

expressionBlock :: TokenParser Block
expressionBlock = p <?> "ExpressionBlock"
  where
    p = withPosition do
      openBlock
      expression' <- expression
      closeBlock
      return $ ExpressionBlock expression'

layoutBlock :: TokenParser Block
layoutBlock = p <?> "LayoutBlock"
  where
    p = withPosition do
      openBlock
      withToken \case
        OpenLayoutToken {} -> Just ()
        _ -> Nothing
      expression' <- expression
      closeBlock
      return $ LayoutBlock expression'

templateEndBlock :: TokenParser Block
templateEndBlock = p <?> "EndTemplateBlock"
  where
    p = withPosition do
      openBlock
      withToken \case
        EndToken {} -> Just ()
        _ -> Nothing
      closeBlock
      return TemplateEndBlock

templateDefaultBlock :: TokenParser Block
templateDefaultBlock = p <?> "DefaultTemplateBlock"
  where
    p = withPosition do
      openBlock
      else'
      closeBlock
      return TemplateElseBlock

templateNextBlock :: TokenParser Block
templateNextBlock = p <?> "NextTemplateBlock"
  where
    p = withPosition do
      openBlock
      else'
      expression' <- expression
      closeBlock
      return $ TemplateNextBlock expression'

else' :: TokenParser ()
else' = withToken \case
  ElseToken {} -> Just ()
  _ -> Nothing

templateStartBlock :: TokenParser Block
templateStartBlock = p <?> "StartTemplateBlock"
  where
    p = withPosition do
      openBlock
      withToken \case
        OpenTemplateToken {} -> Just ()
        _ -> Nothing
      expression' <- expression
      closeBlock
      return $ TemplateStartBlock expression'

textBlock :: TokenParser Block
textBlock = p <?> "TextBlock"
  where
    p = uncurry TextBlock <$> requireText

expression :: TokenParser Expression
expression = filterExpression <?> "Expression"
{-# INLINE expression #-}

filterExpression :: TokenParser Expression
filterExpression = applyExpression `chainl1` filtered <?> "FilterExpression"
  where
    filtered = do
      pos <- withToken \case
        PipeToken pos -> Just pos
        _ -> Nothing
      return $ f pos
    f pos x y = FilterExpression x y pos

applyExpression :: TokenParser Expression
applyExpression = chain . NEL.fromList <$> many1 accessExpression <?> "ApplyExpression"
  where
    chain (fn :| args) = chain' fn args
    chain' fn (arg : rest) = chain' (ApplyExpression fn arg (getExpressionPos arg)) rest
    chain' done [] = done

accessExpression :: TokenParser Expression
accessExpression = simpleExpression `chainl1` accessed <?> "AccessExpression"
  where
    accessed = do
      pos <- withToken \case
        DotToken pos -> Just pos
        _ -> Nothing
      return $ f pos
    f pos x y = AccessExpression x y pos

simpleExpression :: TokenParser Expression
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

stringExpression :: TokenParser Expression
stringExpression = p <?> "StringLiteral"
  where
    p = withToken \case
      StringToken value pos -> Just (StringExpression value pos)
      _ -> Nothing

intExpression :: TokenParser Expression
intExpression = p <?> "IntLiteral"
  where
    p = withToken \case
      IntToken value pos -> Just (IntExpression value pos)
      _ -> Nothing

doubleExpression :: TokenParser Expression
doubleExpression = p <?> "DoubleLiteral"
  where
    p = withToken \case
      DoubleToken value pos -> Just (DoubleExpression value pos)
      _ -> Nothing

boolExpression :: TokenParser Expression
boolExpression = p <?> "BoolLiteral"
  where
    p = withToken \case
      BoolToken value pos -> Just (BoolExpression value pos)
      _ -> Nothing

nameExpression :: TokenParser Expression
nameExpression = p <?> "Name"
  where
    p = withToken \case
      NameToken value pos -> Just (NameExpression value pos)
      _ -> Nothing

parensExpression :: TokenParser Expression
parensExpression = do
  withToken \case
    OpenParenToken {} -> Just ()
    _ -> Nothing
  expression' <- expression
  withToken \case
    CloseParenToken {} -> Just ()
    _ -> Nothing
  return expression'

contextExpression :: TokenParser Expression
contextExpression = p <?> "ContextLiteral"
  where
    p = withPosition do
      withToken \case
        OpenBraceToken {} -> Just ()
        _ -> Nothing
      pairs <- contextKeyValue `sepBy` comma
      withToken \case
        CloseBraceToken {} -> Just ()
        _ -> Nothing
      return $ ContextExpression pairs

contextKeyValue :: TokenParser (String, Expression)
contextKeyValue = p <?> "Pair"
  where
    p = do
      key <- withToken \case
        NameToken key _ -> Just key
        _ -> Nothing
      withToken \case
        ColonToken {} -> Just ()
        _ -> Nothing
      value <- expression
      return (key, value)

listExpression :: TokenParser Expression
listExpression = p <?> "ListLiteral"
  where
    p = withPosition do
      withToken \case
        OpenBracketToken {} -> Just ()
        _ -> Nothing
      values <- expression `sepEndBy` comma
      withToken \case
        CloseBracketToken {} -> Just ()
        _ -> Nothing
      return $ ListExpression values

comma :: TokenParser ()
comma =
  () <$ withToken \case
    CommaToken {} -> Just ()
    _ -> Nothing

requireText :: TokenParser (String, SourcePos)
requireText = withToken f <?> "Text"
  where
    f = \case
      TextToken value pos -> Just (value, pos)
      _ -> Nothing

withToken :: (Token -> Maybe a) -> Parsec [Token] u a
withToken = P.token show getTokenPos

token :: Lexer Token
token =
  getLexerMode >>= \case
    BlockMode ->
      blockToken >>= \case
        t@CloseBlockToken {} -> do
          startText
          return t
        t@OpenCommentToken {} -> do
          startComment
          return t
        t -> return t
    TextMode ->
      choice
        [ expectBlockOpen >> startBlock >> token,
          textToken <* startBlock
        ]
    CommentMode {} -> commentText

startText :: (Show t, Stream s m t) => ParsecT s ParserState m ()
startText = putLexerMode TextMode <?> "StartText"

startBlock :: (Show t, Stream s m t) => ParsecT s ParserState m ()
startBlock = putLexerMode BlockMode <?> "StartBlock"

startComment :: (Show t, Stream s m t) => ParsecT s ParserState m ()
startComment = putLexerMode (CommentMode 0) <?> "StartComment"

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

commentText :: Lexer Token
commentText = withPosition (TextToken <$> p "") <?> "CommentToken"
  where
    p acc =
      choice
        [ downComment acc,
          upComment acc,
          text >>= p . (acc ++)
        ]

    downComment acc = do
      expectBlockOpen
      n <- commentLevel
      putLexerMode $ CommentMode (n + 1)
      string "{{" >>= p . (acc ++)

    upComment acc = do
      expectBlockClose
      commentLevel >>= \case
        0 -> do
          startBlock
          return acc
        n -> do
          putLexerMode $ CommentMode (n - 1)
          string "}}" >>= p . (acc ++)

    commentLevel = do
      getLexerMode >>= \case
        CommentMode n -> return n
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
    justBrace = tryOne [openBraceChar, closeBraceChar] <?> "Brace"
    blockEscape = char '\\' *> (string "{{" <|> string "}}") <?> "BlockEscape"

symbolToken :: Lexer Token
symbolToken =
  withPosition $ tryOne (fmap ws trimmed ++ untrimmed)
  where
    untrimmed =
      [ CloseBlockToken <$ string "}}" <?> "CloseBlockToken '}}'",
        OpenCommentToken <$ char '!' <?> "CommentBlockToken '!'"
      ]
    trimmed =
      [ OpenParenToken <$ char '(' <?> "OpenParenToken '('",
        CloseParenToken <$ char ')' <?> "CloseParenToken ')'",
        OpenBraceToken <$ openBraceChar <?> "OpenBraceToken '{'",
        CloseBraceToken <$ closeBraceChar <?> "CloseBraceToken '}'",
        OpenBracketToken <$ char '[' <?> "OpenBracketToken '['",
        CloseBracketToken <$ char ']' <?> "OpenBracketToken ']'",
        OpenBlockToken <$ string "{{" <?> "OpenBlockToken '{{'",
        OpenTemplateToken <$ char '#' <?> "TemplateBlockToken '#'",
        OpenLayoutToken <$ char '@' <?> "LayoutBlockToken '@'",
        PipeToken <$ char '|' <?> "PipeToken '|'",
        ColonToken <$ char ':' <?> "ColonToken ':'",
        DotToken <$ char '.' <?> "DotToken '.'",
        CommaToken <$ char ',' <?> "CommaToken ','"
      ]

expectBlockOpen :: Lexer ()
expectBlockOpen = () <$ lookAhead (try (string "{{"))

expectBlockClose :: Lexer ()
expectBlockClose = () <$ lookAhead (try (string "}}"))

boolToken :: Lexer Token
boolToken = withPosition (BoolToken <$> value) <?> "BoolToken"
  where
    value =
      True <$ keyword "true"
        <|> False <$ keyword "false"

stringToken :: Lexer Token
stringToken = withPosition $ ws (StringToken <$> stringChars <?> "StringToken")
  where
    stringChars = between dquote dquote (many (stringChar <|> escapeChar)) <?> "StringChars"
    dquote = char '"' <?> "DoubleQuote '\"'"
    stringChar = noneOf "\\\"\n" <?> "StringChar"
    escapeChar = p <?> "EscapeChar"
      where
        p =
          char '\\'
            *> tryOne
              [ char '\\',
                char '\'',
                char '"',
                char 'n' $> '\n',
                char 't' $> '\t'
              ]

numberToken :: Lexer Token
numberToken = withPosition (ws numberToken')
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
nameToken = withPosition $ ws (NameToken <$> name <?> "NameToken")

openBraceChar :: Lexer Char
openBraceChar = char '{' <* notFollowedBy (char '{') <?> "JustOpenBrace '{'"

closeBraceChar :: Lexer Char
closeBraceChar = char '}' <* notFollowedBy (char '}') <?> "JustCloseBrace '}'"

endKeyword :: Lexer Token
endKeyword = withPosition (EndToken <$ keyword "end")

elseKeyword :: Lexer Token
elseKeyword = withPosition (ElseToken <$ keyword "else")

keyword :: String -> Lexer ()
keyword s = ws p
  where
    p = do
      _ <- string s <?> "Keyword '" ++ s ++ "'"
      notFollowedBy nameRest
      return ()

name :: Lexer String
name = p <?> "NameValue"
  where
    p = ws do
      start <- nameStart
      rest <- many nameRest
      return (start : rest)

nameStart :: Lexer Char
nameStart = oneOf ('_' : ['a' .. 'z']) <?> "NameStart"

nameRest :: Lexer Char
nameRest = tryOne [nameStart, alphaNum, char '-'] <?> "NameRest"

ws :: Lexer a -> Lexer a
ws = (<* spaces)

tryOne :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
tryOne = choice . fmap try

withPosition :: (Stream s m t) => ParsecT s u m (SourcePos -> a) -> ParsecT s u m a
withPosition p = do
  pos <- getPosition
  f <- p
  return $ f pos

getIsDebugging :: (Monad m) => ParsecT s ParserState m Bool
getIsDebugging = parserStateIsDebugging <$> getState

getLexerMode :: (Monad m) => ParsecT s ParserState m LexerMode
getLexerMode = parserStateLexerMode <$> getState

putLexerMode :: (Monad m) => LexerMode -> ParsecT s ParserState m ()
putLexerMode mode = do
  state <- getState
  putState state {parserStateLexerMode = mode}

(<?>) :: (Show t, Stream s m t) => ParsecT s ParserState m a -> String -> ParsecT s ParserState m a
p <?> label' = traced' label' (p P.<?> label')

infix 0 <?>

whenDebugging :: (Stream s m t) => ParsecT s ParserState m () -> ParsecT s ParserState m ()
whenDebugging p = do
  isDebugging <- getIsDebugging
  when isDebugging p

trace' :: (Show t, Stream s m t) => String -> ParsecT s ParserState m ()
trace' label' = whenDebugging $ parserTrace label'

traced' :: (Show t, Stream s m t) => String -> ParsecT s ParserState m a -> ParsecT s ParserState m a
traced' label' p = do
  isDebugging <- getIsDebugging
  if isDebugging
    then parserTraced label' p
    else p

data Token
  = OpenBlockToken SourcePos -- "{{"
  | CloseBlockToken SourcePos -- "}}"
  | OpenLayoutToken SourcePos -- "@"
  | OpenTemplateToken SourcePos -- "#"
  | OpenCommentToken SourcePos -- "!"
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
  | StringToken String SourcePos -- '"' [^"]* '"'
  | IntToken Int SourcePos -- 0|[1-9][0-9]*
  | DoubleToken Double SourcePos -- 0|[1-9][0-9]*\.[0-9]+
  | TextToken String SourcePos
  deriving stock (Eq, Show)

getTokenPos :: Token -> SourcePos
getTokenPos = \case
  OpenBlockToken pos -> pos
  CloseBlockToken pos -> pos
  OpenLayoutToken pos -> pos
  OpenTemplateToken pos -> pos
  OpenCommentToken pos -> pos
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
