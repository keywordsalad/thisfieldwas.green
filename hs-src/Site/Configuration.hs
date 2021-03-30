module Site.Configuration where

import Hakyll
import System.FilePath (takeFileName)

hakyllConfiguration :: Configuration
hakyllConfiguration = defaultConfiguration
  { providerDirectory = "site-src"
  , destinationDirectory = "gh-pages"
  , ignoreFile = customIgnoreFile
  }

customIgnoreFile :: FilePath -> Bool
customIgnoreFile path =
  ignoreFile defaultConfiguration path && not (fileName `elem` allowedFiles)
  where
    fileName = takeFileName path
    allowedFiles = [".nojekyll"]

feedConfig :: String -> FeedConfiguration
feedConfig siteRoot =
  FeedConfiguration
    { feedTitle = "This Field Was Green"
    , feedDescription = ""
    , feedAuthorName = "Logan McGrath"
    , feedAuthorEmail = "blog@thisfieldwas.green"
    , feedRoot = siteRoot
    }
