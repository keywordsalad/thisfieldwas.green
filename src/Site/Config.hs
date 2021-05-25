module Site.Config where

import Control.Applicative ((<|>))
import Data.Ini.Config
import Data.Maybe (isJust)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time
import Hakyll
import Hakyll.Core.Configuration as HC
import Lens.Micro
import Lens.Micro.TH
import System.FilePath

data SiteConfig = SiteConfig
  { _siteEnv :: [(String, String)],
    _siteRoot :: String,
    _siteTitle :: String,
    _siteAuthorName :: String,
    _siteAuthorEmail :: String,
    _siteLinkedInProfile :: String,
    _siteGitWebUrl :: String,
    _sitePreview :: Bool,
    _siteDebug :: Bool,
    _siteHakyllConfiguration :: Configuration,
    _siteFeedConfiguration :: FeedConfiguration,
    _siteTime :: ZonedTime,
    _siteContext :: Context String
  }

makeLenses ''SiteConfig

makeHakyllConfigLens :: (Configuration -> a) -> (Configuration -> a -> Configuration) -> Lens' SiteConfig a
makeHakyllConfigLens getter setter = lens getter' setter'
  where
    getter' siteConfig = getter (siteConfig ^. siteHakyllConfiguration)
    setter' siteConfig val = siteConfig & siteHakyllConfiguration %~ flip setter val

siteDestinationDirectory :: Lens' SiteConfig FilePath
siteDestinationDirectory = makeHakyllConfigLens destinationDirectory (\h v -> h {destinationDirectory = v})

siteProviderDirectory :: Lens' SiteConfig FilePath
siteProviderDirectory = makeHakyllConfigLens providerDirectory (\h v -> h {providerDirectory = v})

siteStoreDirectory :: Lens' SiteConfig FilePath
siteStoreDirectory = makeHakyllConfigLens storeDirectory (\h v -> h {storeDirectory = v})

siteInMemoryCache :: Lens' SiteConfig Bool
siteInMemoryCache = makeHakyllConfigLens inMemoryCache (\h v -> h {inMemoryCache = v})

hasEnvFlag :: String -> [(String, String)] -> Bool
hasEnvFlag f e = isJust (lookup f e)

parseConfigIni :: [(String, String)] -> ZonedTime -> Text -> Either String SiteConfig
parseConfigIni env zonedTime iniText = parseIniFile iniText do
  feedDescription <- section "feed" $ fieldOf "description" string

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

  section "site" do
    root <- fieldOf "root" string
    title <- fieldOf "title" string
    authorName <- fieldOf "authorName" string
    authorEmail <- fieldOf "authorEmail" string
    linkedInProfile <- fieldOf "linkedInProfile" string
    preview <- fieldDefOf "preview" flag False <|> return (hasEnvFlag "SITE_PREVIEW" env)
    debug <- fieldDefOf "debug" flag False <|> return (hasEnvFlag "SITE_DEBUG" env)
    gitWebUrl <- fieldOf "gitWebUrl" string
    return
      SiteConfig
        { _siteEnv = env,
          _siteRoot = root,
          _siteTitle = title,
          _siteAuthorName = authorName,
          _siteAuthorEmail = authorEmail,
          _siteLinkedInProfile = linkedInProfile,
          _siteGitWebUrl = gitWebUrl,
          _sitePreview = preview,
          _siteDebug = debug,
          _siteHakyllConfiguration = hakyllConfiguration,
          _siteFeedConfiguration =
            FeedConfiguration
              { feedTitle = title,
                feedDescription = feedDescription,
                feedAuthorName = authorName,
                feedAuthorEmail = authorEmail,
                feedRoot = root
              },
          _siteTime = zonedTime,
          _siteContext = defaultContext
        }
  where
    customIgnoreFile allowedFiles path =
      ignoreFile defaultConfiguration path && fileName `notElem` allowedFiles
      where
        fileName = takeFileName path

fieldOfStrings :: IsString a => Text -> SectionParser [a]
fieldOfStrings k = fieldDefOf k (listWithSeparator "," string) []
