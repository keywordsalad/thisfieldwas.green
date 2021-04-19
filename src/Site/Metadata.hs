module Site.Metadata where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.String.Utils as S
import qualified Data.Text as T
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    iso8601DateFormat,
    parseTimeM,
  )
import GHC.Generics

data PageMetadata = PageMetadata
  { contentTemplates :: [String],
    templates :: [String],
    title :: String,
    author :: Maybe String,
    created :: UTCTime,
    published :: UTCTime,
    tags :: [String],
    comments :: Bool
  }
  deriving stock (Generic, Show)

data PageMetadataConfig = PageMetadataConfig
  { defaultContentTemplates :: [String],
    defaultTemplates :: [String],
    defaultTitle :: String
  }

defaultPageMetadataConfig :: PageMetadataConfig
defaultPageMetadataConfig =
  PageMetadataConfig
    { defaultContentTemplates = ["bare-content"],
      defaultTemplates = ["skeleton"],
      defaultTitle = "Untitled"
    }

parsePageMetadata :: PageMetadataConfig -> Object -> Parser PageMetadata
parsePageMetadata config pm =
  PageMetadata
    <$> ifEmpty' (defaultContentTemplates config) (withStringOrList "content-templates" pm)
    <*> ifEmpty' (defaultTemplates config) (withStringOrList "templates" pm)
    <*> ifEmpty' (defaultTitle config) (withField "title" pm)
    <*> withField "author" pm
    <*> (withField "created" pm >>= toDate)
    <*> (withField "published" pm >>= toDate)
    <*> withStringOrList "tags" pm
    <*> withField "comments" pm
  where
    ifEmpty' d mxs = go <$> mxs
      where
        go [] = d
        go xs = xs

withField :: (FromJSON a) => String -> Object -> Parser a
withField k o = o .: k' <?> Key k'
  where
    k' = T.pack k

withStringOrList :: String -> Object -> Parser [String]
withStringOrList k o =
  withString <|> withList
  where
    withString = splitAndStrip "," . T.unpack <$> (withField k o :: Parser T.Text)
    withList = fmap T.unpack <$> (withField k o :: Parser [T.Text])
    splitAndStrip d = fmap S.strip . S.split d

toString :: String -> Value -> Parser String
toString k = withText k (return . T.unpack)

toDate :: (MonadFail m) => String -> m UTCTime
toDate = parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%X%Ez")
