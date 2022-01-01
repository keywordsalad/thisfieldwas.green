module Green.Config where

import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
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

instance ToJSON SiteDebug where
  toJSON SiteDebug {..} =
    object
      [ "preview" .= _debugPreview,
        "inflate-css" .= _debugInflateCss
      ]

data SiteInfo = SiteInfo
  { _siteRoot :: String,
    _siteTitle :: String,
    _siteDescription :: String,
    _siteAuthorName :: String,
    _siteAuthorEmail :: String,
    _siteLinkedInProfile :: String,
    _siteTwitterProfile :: String,
    _siteGitHubProfile :: String,
    _siteGiteaProfile :: String,
    _siteGiteaWebUrl :: String
  }
  deriving stock (Show)

makeLenses ''SiteInfo

siteTwitterHandle :: SimpleGetter SiteInfo String
siteTwitterHandle = to \info ->
  '@' : (reverse . takeWhile (/= '/') . reverse $ info ^. siteTwitterProfile)

instance FromJSON SiteInfo where
  parseJSON = withObject "SiteInfo" \info ->
    SiteInfo
      <$> info .: "root"
      <*> info .: "title"
      <*> info .:? "description" .!= ""
      <*> info .: "author-name"
      <*> info .: "author-email"
      <*> info .: "linkedin-profile"
      <*> info .: "twitter-profile"
      <*> info .: "github-profile"
      <*> info .: "gitea-profile"
      <*> info .: "gitea-web-url"

instance ToJSON SiteInfo where
  toJSON info@SiteInfo {..} =
    object
      [ "root" .= _siteRoot,
        "title" .= _siteTitle,
        "description" .= _siteDescription,
        "author-name" .= _siteAuthorName,
        "author-email" .= _siteAuthorEmail,
        "linkedin-profile" .= _siteLinkedInProfile,
        "twitter-profile" .= _siteTwitterProfile,
        "twitter-handle" .= (info ^. siteTwitterHandle),
        "github-profile" .= _siteGitHubProfile,
        "gitea-profile" .= _siteGiteaProfile,
        "gitea-web-url" .= _siteGiteaWebUrl
      ]

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

instance ToJSON SiteDisplayFormat where
  toJSON SiteDisplayFormat {..} =
    object
      [ "date-long-format" .= _displayDateLongFormat,
        "date-short-format" .= _displayDateShortFormat,
        "time-format" .= _displayTimeFormat,
        "robot-date" .= _displayRobotDate,
        "robot-time" .= _displayRobotTime,
        "image-widths" .= _displayImageWidths
      ]

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

instance ToJSON SiteConfig where
  toJSON SiteConfig {..} =
    object
      [ "current-time" .= _siteCurrentTime,
        "time-locale" .= show _siteTimeLocale,
        "info" .= _siteInfo,
        "debug" .= _siteDebug,
        "hakyll-configuration" .= toHakyllConfigurationJSON _siteHakyllConfiguration,
        "display-format" .= _siteDisplayFormat,
        "env" .= _siteEnv
      ]

instance Show SiteConfig where
  show = Char8.unpack . encode . toJSON

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

toHakyllConfigurationJSON :: H.Configuration -> Value
toHakyllConfigurationJSON H.Configuration {..} =
  object
    [ "provider-directory" .= providerDirectory,
      "destination-directory" .= destinationDirectory,
      "allowed-files" .= ("[consumed]" :: String)
    ]

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
