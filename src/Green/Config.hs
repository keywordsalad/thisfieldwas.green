module Green.Config where

import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Yaml
import Green.Common
import Green.Lens
import qualified Hakyll as H

data SiteDebug = SiteDebug
  { _debugPreview :: Bool,
    _debugInflateCss :: Bool
  }
  deriving stock (Show)

makeLenses ''SiteDebug

defaultSiteDebug :: SiteDebug
defaultSiteDebug =
  SiteDebug
    { _debugPreview = False,
      _debugInflateCss = False
    }

instance FromJSON SiteDebug where
  parseJSON = withObject "SiteDebug" \debug ->
    SiteDebug
      <$> debug .:? "preview" .!= (defaultSiteDebug ^. debugPreview)
      <*> debug .:? "inflate-css" .!= (defaultSiteDebug ^. debugInflateCss)

data SiteInfo = SiteInfo
  { _siteRoot :: String,
    _siteTitle :: String,
    _siteDescription :: String,
    _siteAuthorName :: String,
    _siteAuthorEmail :: String,
    _siteLinkedInProfile :: String,
    _siteGitWebUrl :: String
  }
  deriving stock (Show)

makeLenses ''SiteInfo

instance FromJSON SiteInfo where
  parseJSON = withObject "SiteInfo" \info ->
    SiteInfo
      <$> info .: "root"
      <*> info .: "title"
      <*> info .:? "description" .!= ""
      <*> info .: "author-name"
      <*> info .: "author-email"
      <*> info .: "linkedin-profile"
      <*> info .: "git-web-url"

data SiteDisplayFormat = SiteDisplayFormat
  { _displayDateLongFormat :: String,
    _displayDateShortFormat :: String,
    _displayTimeFormat :: String,
    _displayRobotDate :: String,
    _displayRobotTime :: String,
    _displayImageWidths :: [Int]
  }
  deriving stock (Show)

makeLenses ''SiteDisplayFormat

instance FromJSON SiteDisplayFormat where
  parseJSON = withObject "SiteDisplayFormat" \format ->
    SiteDisplayFormat
      <$> format .: "date-long-format"
      <*> format .: "date-short-format"
      <*> format .: "time-format"
      <*> format .: "robot-date"
      <*> format .: "robot-time"
      <*> format .:? "image-widths" .!= []

data SiteConfig = SiteConfig
  { _siteEnv :: [(String, String)],
    _siteInfo :: SiteInfo,
    _siteDebug :: SiteDebug,
    _siteHakyllConfiguration :: H.Configuration,
    _siteCurrentTime :: ZonedTime,
    _siteTimeLocale :: TimeLocale,
    _siteDisplayFormat :: SiteDisplayFormat
  }

makeLenses ''SiteConfig

siteFeedConfiguration :: SimpleGetter SiteConfig FeedConfiguration
siteFeedConfiguration = to f
  where
    f siteConfig =
      FeedConfiguration
        { feedTitle = siteConfig ^. siteInfo . siteTitle,
          feedRoot = siteConfig ^. siteInfo . siteRoot,
          feedAuthorName = siteConfig ^. siteInfo . siteAuthorName,
          feedAuthorEmail = siteConfig ^. siteInfo . siteAuthorEmail,
          feedDescription = siteConfig ^. siteInfo . siteDescription
        }

siteDestinationDirectory :: Lens' SiteConfig FilePath
siteDestinationDirectory = siteHakyllConfiguration . destinationDirectoryL

siteProviderDirectory :: Lens' SiteConfig FilePath
siteProviderDirectory = siteHakyllConfiguration . providerDirectoryL

siteStoreDirectory :: Lens' SiteConfig FilePath
siteStoreDirectory = siteHakyllConfiguration . storeDirectoryL

siteInMemoryCache :: Lens' SiteConfig Bool
siteInMemoryCache = siteHakyllConfiguration . inMemoryCacheL

sitePreview :: Lens' SiteConfig Bool
sitePreview = siteDebug . debugPreview

instance Show SiteConfig where
  show config =
    intercalate "\n" $
      [ "SiteConfig:",
        "  Time: " <> show (config ^. siteCurrentTime),
        "  SiteInfo:",
        "    Root: " <> show (config ^. siteInfo . siteRoot),
        "    Title: " <> show (config ^. siteInfo . siteTitle),
        "    Description: " <> show (config ^. siteInfo . siteDescription),
        "    AuthorName: " <> show (config ^. siteInfo . siteAuthorName),
        "    AuthorEmail: " <> show (config ^. siteInfo . siteAuthorEmail),
        "    LinkedInProfile: " <> show (config ^. siteInfo . siteLinkedInProfile),
        "    GitWebUrl: " <> show (config ^. siteInfo . siteGitWebUrl),
        "  Debug:",
        "    Preview: " <> show (config ^. siteDebug . debugPreview),
        "    InflateCss: " <> show (config ^. siteDebug . debugInflateCss),
        "  HakyllConfiguration:",
        "    DestinationDirectory: " <> show (config ^. siteDestinationDirectory),
        "    ProviderDirectory: " <> show (config ^. siteProviderDirectory),
        "    StoreDirectory: " <> show (config ^. siteStoreDirectory),
        "    InMemoryCache: " <> show (config ^. siteInMemoryCache),
        "  DisplayFormat:",
        "    DateLongFormat: " <> show (config ^. siteDisplayFormat . displayDateLongFormat),
        "    DateShortFormat: " <> show (config ^. siteDisplayFormat . displayDateShortFormat),
        "    TimeFormat: " <> show (config ^. siteDisplayFormat . displayTimeFormat),
        "    RobotDate: " <> show (config ^. siteDisplayFormat . displayRobotDate),
        "    RobotTime: " <> show (config ^. siteDisplayFormat . displayRobotTime),
        "    ImageWidths: " <> show (config ^. siteDisplayFormat . displayImageWidths),
        "  Env:",
        "    " <> intercalate "\n    " ((\(key, val) -> key <> "=" <> show val) <$> config ^. siteEnv)
      ]

customIgnoreFile :: Foldable t => t FilePath -> FilePath -> Bool
customIgnoreFile allowedFiles path =
  H.ignoreFile H.defaultConfiguration path
    && takeFileName path `notElem` allowedFiles

parseHakyllConfigurationJSON :: Value -> Parser H.Configuration
parseHakyllConfigurationJSON = withObject "Hakyll.Core.Configuration" \config ->
  initConfig
    <$> config .: "provider-directory"
    <*> config .: "destination-directory"
    <*> config .:? "allowed-files" .!= []
  where
    initConfig providerDirectory' destinationDirectory' allowedFiles =
      H.defaultConfiguration
        { H.providerDirectory = providerDirectory',
          H.destinationDirectory = destinationDirectory',
          H.ignoreFile = customIgnoreFile allowedFiles
        }

parseSiteConfigJSON :: [(String, String)] -> TimeLocale -> ZonedTime -> Value -> Parser SiteConfig
parseSiteConfigJSON env timeLocale time = withObject "SiteConfig" \allConfig -> do
  config <- allConfig .: T.pack envKey
  SiteConfig env
    <$> config .: "site-info"
    <*> (overrideDebugSettings <$> config .:? "debug-settings" .!= defaultSiteDebug)
    <*> (config .: "hakyll-config" >>= parseHakyllConfigurationJSON)
    <*> pure time
    <*> pure timeLocale
    <*> config .: "display-formats"
  where
    envKey = fromMaybe "default" $ lookup "SITE_ENV" env
    overrideDebugSettings debug =
      debug
        & debugInflateCss %~ (\x -> maybe x read $ lookup "SITE_INFLATE_CSS" env)
        & debugPreview %~ (\x -> maybe x read $ lookup "SITE_PREVIEW" env)

parseConfigYaml :: [(String, String)] -> TimeLocale -> ZonedTime -> ByteString -> Either String SiteConfig
parseConfigYaml env timeLocale time =
  first prettyPrintParseException . decodeEither'
    >=> parseEither (parseSiteConfigJSON env timeLocale time)
