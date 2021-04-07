module Site.Metadata where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.String.Utils as S
import qualified Data.Text as T
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    iso8601DateFormat,
    parseTimeM,
  )
import qualified Data.Vector as V
import GHC.Generics

data PostMetadata = PostMetadata
  { contentTemplates :: [String],
    templates :: [String],
    title :: String,
    author :: Maybe String,
    created :: UTCTime,
    date :: Maybe UTCTime,
    comments :: Bool,
    published :: Bool,
    tags :: [String]
  }
  deriving stock (Generic, Show)

instance ToJSON PostMetadata

instance FromJSON PostMetadata where
  parseJSON = withObject "PostMetadata" parsePostMetadata

parsePostMetadata :: Object -> Parser PostMetadata
parsePostMetadata pm =
  PostMetadata
    <$> withOneOrMoreStrings "content-templates" "bare-content" pm
    <*> withOneOrMoreStrings "templates" "skeleton" pm
    <*> (withMaybeField "title" toString pm <&> fromMaybe "Untitled")
    <*> withMaybeField "author" toString pm
    <*> withField "created" toDate pm
    <*> withMaybeField "date" toDate pm
    <*> withField "comments" (`withBool` return) pm
    <*> withField "published" (`withBool` return) pm
    <*> withZeroOrMoreStrings "tags" pm

withZeroOrMoreStrings ::
  -- | the field to parse
  String ->
  -- | the object with the field
  Object ->
  -- | the new parser
  Parser [String]
withZeroOrMoreStrings k o =
  multiple <|> single
  where
    multiple = withZeroOrMore k toString o
    single = withField k toString o <&> fmap S.strip . S.split ","

withOneOrMoreStrings ::
  -- | the field to parse
  String ->
  -- | the default value if the field is empty
  String ->
  -- | the object with the field
  Object ->
  -- | the new parser
  Parser [String]
withOneOrMoreStrings k d o =
  withZeroOrMoreStrings k o <&> \case
    [] -> [d]
    ss -> ss

withScalarOrOneOrMore :: String -> a -> (String -> Value -> Parser a) -> Object -> Parser [a]
withScalarOrOneOrMore k d f o = withOneOrMore k d f o <|> return <$> withField k f o

withField :: String -> (String -> Value -> Parser a) -> Object -> Parser a
withField k f o = fromJust <$> withMaybeField k f o

withMaybeField :: String -> (String -> Value -> Parser a) -> Object -> Parser (Maybe a)
withMaybeField k f o = sequence $ f k <$> M.lookup (T.pack k) o

withOneOrMore :: String -> a -> (String -> Value -> Parser a) -> Object -> Parser [a]
withOneOrMore k d f o =
  withZeroOrMore k f o <&> \case
    [] -> [d]
    xs -> xs

withZeroOrMore :: String -> (String -> Value -> Parser a) -> Object -> Parser [a]
withZeroOrMore k f o = fromMaybe [] <$> withMaybeField k f' o
  where
    f' _ = withArray k f''
    f'' a = sequence $ V.toList $ f k <$> a

toString :: String -> Value -> Parser String
toString k = withText k (return . T.unpack)

toDate :: String -> Value -> Parser UTCTime
toDate k = withText k (parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%X %EZ") . T.unpack)
