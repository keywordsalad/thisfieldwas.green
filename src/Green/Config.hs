module Green.Config where

import Data.Ini.Config
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Green.Common
import Green.Lens.Hakyll
import Hakyll.Core.Configuration as HC

data SiteDebug = SiteDebug
  { _debugPrintItem :: Bool,
    _debugRawCss :: Bool
    -- new fields should be appended, do not rearrange
  }

makeLenses ''SiteDebug

defaultSiteDebug :: SiteDebug
defaultSiteDebug =
  SiteDebug
    { _debugPrintItem = False,
      _debugRawCss = False
    }

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
    _siteTime :: ZonedTime,
    _siteContext :: Context String,
    _siteTimeLocale :: TimeLocale
    -- new fields should be appended, do not rearrange
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

parseConfigIni :: [(String, String)] -> TimeLocale -> ZonedTime -> Text -> Either String SiteConfig
parseConfigIni env timeLocale zonedTime iniText = parseIniFile iniText do
  hakyllConfiguration <- section "hakyll" do
    providerDirectory' <- fieldOf "providerDirectory" string
    destinationDirectory' <- fieldOf "destinationDirectory" string
    allowedFiles <- fieldOfStrings "allowedFiles"
    return
      HC.defaultConfiguration
        { providerDirectory = providerDirectory',
          destinationDirectory = destinationDirectory',
          ignoreFile = customIgnoreFile allowedFiles
        }

  debugSettings <- sectionDef "debug" defaultSiteDebug do
    SiteDebug
      <$> configFlag "printItems" "SITE_PREVIEW" False env
      <*> configFlag "rawCss" "SITE_RAW_CSS" False env

  section "site" do
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
      <*> pure zonedTime
      <*> pure mempty
      <*> pure timeLocale
  where
    customIgnoreFile allowedFiles path =
      ignoreFile defaultConfiguration path
        && takeFileName path `notElem` allowedFiles

fieldOfStrings :: IsString a => Text -> SectionParser [a]
fieldOfStrings k = fieldDefOf k (listWithSeparator "," string) []

configFlag :: String -> String -> Bool -> [(String, String)] -> SectionParser Bool
configFlag configKey envKey defaultValue env =
  case lookup envKey env of
    Just _ -> return True
    Nothing -> fieldFlagDef (T.pack configKey) defaultValue
