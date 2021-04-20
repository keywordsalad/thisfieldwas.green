module Site.Config where

import Control.Applicative ((<|>))
import Data.Ini.Config
import Data.Maybe (isJust)
import Data.String (IsString)
import Data.Text (Text)
import Hakyll
import Hakyll.Core.Configuration as HC
import Lens.Micro.TH
import System.FilePath

data SiteConfig = SiteConfig
  { _siteEnv :: [(String, String)],
    _siteRoot :: String,
    _siteTitle :: String,
    _siteAuthorName :: String,
    _siteAuthorEmail :: String,
    _siteGitWebUrl :: String,
    _sitePreview :: Bool,
    _siteDebug :: Bool,
    _siteHakyllConfiguration :: Configuration,
    _siteFeedConfiguration :: FeedConfiguration,
    _siteContext :: Context String
  }

makeLenses ''SiteConfig

hasEnvFlag :: String -> [(String, String)] -> Bool
hasEnvFlag f e = isJust (lookup f e)

parseConfigIni :: [(String, String)] -> Text -> Either String SiteConfig
parseConfigIni env iniText = parseIniFile iniText do
  feedDescription <- section "feed" $ fieldOf "description" string

  hakyllConfiguration <- section "hakyll" do
    destinationDir <- fieldOf "destinationDirectory" string
    allowedFiles <- fieldOfStrings "allowedFiles"
    return
      HC.defaultConfiguration
        { destinationDirectory = destinationDir,
          ignoreFile = customIgnoreFile allowedFiles
        }

  section "site" do
    root <- fieldOf "root" string
    title <- fieldOf "title" string
    authorName <- fieldOf "authorName" string
    authorEmail <- fieldOf "authorEmail" string
    preview <- fieldDefOf "preview" flag False <|> pure (hasEnvFlag "SITE_PREVIEW" env)
    debug <- fieldDefOf "debug" flag False <|> pure (hasEnvFlag "SITE_DEBUG" env)
    gitWebUrl <- fieldOf "gitWebUrl" string
    return
      SiteConfig
        { _siteEnv = env,
          _siteRoot = root,
          _siteTitle = title,
          _siteAuthorName = authorName,
          _siteAuthorEmail = authorEmail,
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
          _siteContext = defaultContext
        }
  where
    customIgnoreFile allowedFiles path =
      ignoreFile defaultConfiguration path && fileName `notElem` allowedFiles
      where
        fileName = takeFileName path

fieldOfStrings :: IsString a => Text -> SectionParser [a]
fieldOfStrings k = fieldDefOf k (listWithSeparator "," string) []
