module Green.Config where

import Data.Ini.Config
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Green.Common
import Green.Lens
import Hakyll.Core.Configuration as HC

data SiteDebug = SiteDebug
  { _debugPrintItem :: Maybe Identifier,
    _debugRawCss :: Bool
  }

makeLenses ''SiteDebug

defaultSiteDebug :: SiteDebug
defaultSiteDebug =
  SiteDebug
    { _debugPrintItem = Nothing,
      _debugRawCss = False
    }

data SiteDisplayFormat = SiteDisplayFormat
  { _displayDateLongFormat :: String,
    _displayDateShortFormat :: String,
    _displayTimeFormat :: String
  }

makeLenses ''SiteDisplayFormat

data SiteConfig = SiteConfig
  { _siteEnv :: [(String, String)],
    _siteRoot :: String,
    _siteTitle :: String,
    _siteDescription :: String,
    _siteAuthorName :: String,
    _siteAuthorEmail :: String,
    _siteLinkedInProfile :: String,
    _siteGitWebUrl :: String,
    _siteDebug :: SiteDebug,
    _siteHakyllConfiguration :: Configuration,
    _siteTime :: LocalTime,
    _siteContext :: Context String,
    _siteTimeLocale :: TimeLocale,
    _siteDisplayFormat :: SiteDisplayFormat
  }

makeLenses ''SiteConfig

siteFeedConfiguration :: SimpleGetter SiteConfig FeedConfiguration
siteFeedConfiguration = to f
  where
    f siteConfig =
      FeedConfiguration
        { feedTitle = siteConfig ^. siteTitle,
          feedRoot = siteConfig ^. siteRoot,
          feedAuthorName = siteConfig ^. siteAuthorName,
          feedAuthorEmail = siteConfig ^. siteAuthorEmail,
          feedDescription = siteConfig ^. siteDescription
        }

siteDestinationDirectory :: Lens' SiteConfig FilePath
siteDestinationDirectory = siteHakyllConfiguration . destinationDirectoryL

siteProviderDirectory :: Lens' SiteConfig FilePath
siteProviderDirectory = siteHakyllConfiguration . providerDirectoryL

siteStoreDirectory :: Lens' SiteConfig FilePath
siteStoreDirectory = siteHakyllConfiguration . storeDirectoryL

siteInMemoryCache :: Lens' SiteConfig Bool
siteInMemoryCache = siteHakyllConfiguration . inMemoryCacheL

hasEnvFlag :: String -> [(String, String)] -> Bool
hasEnvFlag f e = isJust (lookup f e)

parseConfigIni :: [(String, String)] -> TimeLocale -> LocalTime -> Text -> Either String SiteConfig
parseConfigIni env timeLocale time iniText = parseIniFile iniText do
  hakyllConfiguration <- section "Hakyll" do
    providerDirectory' <- fieldOf "providerDirectory" string
    destinationDirectory' <- fieldOf "destinationDirectory" string
    allowedFiles <- fieldOfStrings "allowedFiles"
    return
      HC.defaultConfiguration
        { providerDirectory = providerDirectory',
          destinationDirectory = destinationDirectory',
          ignoreFile = customIgnoreFile allowedFiles
        }

  debugSettings <- sectionDef "Debug" defaultSiteDebug do
    SiteDebug
      <$> configEnvMbOf "printItems" "SITE_PREVIEW" string env
      <*> configEnvFlag "rawCss" "SITE_RAW_CSS" False env

  displayFormat <- section "DisplayFormats" do
    SiteDisplayFormat
      <$> fieldOf "dateLongFormat" string
      <*> fieldOf "dateShortFormat" string
      <*> fieldOf "timeFormat" string

  section "Site" do
    SiteConfig env
      <$> fieldOf "root" string
      <*> fieldOf "title" string
      <*> (fieldOf "description" string <|> return "")
      <*> fieldOf "authorName" string
      <*> fieldOf "authorEmail" string
      <*> fieldOf "linkedInProfile" string
      <*> fieldOf "gitWebUrl" string
      <*> pure debugSettings
      <*> pure hakyllConfiguration
      <*> pure time
      <*> pure mempty
      <*> pure timeLocale
      <*> pure displayFormat
  where
    customIgnoreFile allowedFiles path =
      ignoreFile defaultConfiguration path
        && takeFileName path `notElem` allowedFiles

fieldOfStrings :: IsString a => Text -> SectionParser [a]
fieldOfStrings k = fieldDefOf k (listWithSeparator "," string) []

configEnvFlag :: String -> String -> Bool -> [(String, String)] -> SectionParser Bool
configEnvFlag configKey envKey defaultValue env =
  case lookup envKey env of
    Just _ -> return True
    Nothing -> fieldFlagDef (T.pack configKey) defaultValue

configEnvMbOf :: String -> String -> (Text -> Either String a) -> [(String, String)] -> SectionParser (Maybe a)
configEnvMbOf configKey envKey parse env =
  fieldFromEnv <|> fieldMbOf (T.pack configKey) parse
  where
    fieldFromEnv = sequence $ getValue . parse . T.pack <$> lookup envKey env
    getValue (Left e) = error e
    getValue (Right v) = return v
