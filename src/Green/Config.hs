module Green.Config where

import Data.Ini.Config
import Data.Text (Text)
import qualified Data.Text as T
import Green.Common
import Green.Lens
import Hakyll.Core.Configuration as HC
import Hakyll.Core.Identifier.Pattern ((.||.))

data SiteDebug = SiteDebug
  { _debugPreview :: Bool,
    _debugRawCss :: Bool
  }

makeLenses ''SiteDebug

defaultSiteDebug :: SiteDebug
defaultSiteDebug =
  SiteDebug
    { _debugPreview = False,
      _debugRawCss = False
    }

data SiteDisplayFormat = SiteDisplayFormat
  { _displayDateLongFormat :: String,
    _displayDateShortFormat :: String,
    _displayTimeFormat :: String,
    _displayImageWidths :: [Int]
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
    _siteTime :: ZonedTime,
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

sitePostsPattern :: SimpleGetter SiteConfig Pattern
sitePostsPattern = to f
  where
    f config
      | config ^. siteDebug . debugPreview = "_posts/**" .||. "_drafts/**"
      | otherwise = "_posts/**"

hasEnvFlag :: String -> [(String, String)] -> Bool
hasEnvFlag f e = isJust (lookup f e)

parseConfigIni :: [(String, String)] -> TimeLocale -> ZonedTime -> Text -> Either String SiteConfig
parseConfigIni env timeLocale time iniText = parseIniFile iniText do
  hakyllConfiguration <- section "Hakyll" do
    providerDirectory' <- fieldOf "providerDirectory" string
    destinationDirectory' <- fieldOf "destinationDirectory" string
    allowedFiles <- fieldListOf "allowedFiles" string
    return
      HC.defaultConfiguration
        { providerDirectory = providerDirectory',
          destinationDirectory = destinationDirectory',
          ignoreFile = customIgnoreFile allowedFiles
        }

  debugSettings <- sectionDef "Debug" defaultSiteDebug do
    SiteDebug
      <$> configEnvFlag "preview" "DEBUG_PREVIEW" False env
      <*> configEnvFlag "rawCss" "DEBUG_RAW_CSS" False env

  displayFormat <- section "DisplayFormats" do
    SiteDisplayFormat
      <$> fieldOf "dateLongFormat" string
      <*> fieldOf "dateShortFormat" string
      <*> fieldOf "timeFormat" string
      <*> fieldListOf "imageWidths" number

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
      <*> pure timeLocale
      <*> pure displayFormat
  where
    customIgnoreFile allowedFiles path =
      ignoreFile defaultConfiguration path
        && takeFileName path `notElem` allowedFiles

fieldListOf :: Text -> (Text -> Either String a) -> SectionParser [a]
fieldListOf k p = fieldDefOf k (listWithSeparator "," p) []

configEnvFlag :: String -> String -> Bool -> [(String, String)] -> SectionParser Bool
configEnvFlag configKey envKey defaultValue env =
  case lookup envKey env of
    Just _ -> return True
    Nothing -> fieldFlagDef (T.pack configKey) defaultValue

configEnvMbOf :: String -> String -> (Text -> Either String a) -> [(String, String)] -> SectionParser (Maybe a)
configEnvMbOf configKey envKey parseFn env =
  fieldFromEnv <|> fieldMbOf (T.pack configKey) parseFn
  where
    fieldFromEnv = sequence $ getValue . parseFn . T.pack <$> lookup envKey env
    getValue (Left e) = error e
    getValue (Right v) = return v
