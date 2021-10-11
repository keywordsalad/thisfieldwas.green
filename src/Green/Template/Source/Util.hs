module Green.Template.Source.Util where

import Control.Monad.Identity
import Data.Default
import Text.Parsec hiding (label, runParser, token, (<?>))
import qualified Text.Parsec as P

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

data LexerMode
  = TextMode
  | BlockMode
  | FencedMode Int
  deriving stock (Show)

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

labeled :: (Show t, Stream s m t) => String -> ParsecT s ParserState m a -> ParsecT s ParserState m a
labeled = flip (<?>)

(<?>) :: (Show t, Stream s m t) => ParsecT s ParserState m a -> String -> ParsecT s ParserState m a
p <?> label = traced label (p P.<?> label)

infix 0 <?>

whenDebugging :: (Stream s m t) => ParsecT s ParserState m () -> ParsecT s ParserState m ()
whenDebugging p = do
  isDebugging <- getIsDebugging
  when isDebugging p

trace :: (Show t, Stream s m t) => String -> ParsecT s ParserState m ()
trace label = whenDebugging $ parserTrace label

traced :: (Show t, Stream s m t) => String -> ParsecT s ParserState m a -> ParsecT s ParserState m a
traced label p =
  getIsDebugging >>= \case
    True -> parserTraced label p
    False -> p

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
