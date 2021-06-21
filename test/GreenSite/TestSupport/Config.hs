module GreenSite.TestSupport.Config where

import Data.Time
import Hakyll as H
import GreenSite.Common

defaultTestTimeString :: String
defaultTestTimeString = "2013-06-16T21:12:00-07:00"

defaultTestTime :: (MonadFail m) => m ZonedTime
defaultTestTime = timeFromString defaultTestTimeString

timeFromString :: (MonadFail m) => String -> m ZonedTime
timeFromString = parseTimeM True defaultTimeLocale "%FT%T%EZ"

defaultFeedConfig :: H.FeedConfiguration
defaultFeedConfig =
  H.FeedConfiguration
    { feedTitle = "This Old Feed",
      feedDescription = "Feeding the old worm the good stuff",
      feedAuthorName = "Slurms McKenzie",
      feedAuthorEmail = "slurms@thisold.blog",
      feedRoot = "https://thisold.blog"
    }

defaultHakyllConfig :: H.Configuration
defaultHakyllConfig =
  defaultConfiguration
    { destinationDirectory = "_test/site",
      storeDirectory = "_test/store",
      tmpDirectory = "_test/tmp",
      providerDirectory = "test/data"
    }

defaultSiteConfig :: SiteConfig
defaultSiteConfig = defaultSiteConfigWith defaultHakyllConfig

defaultSiteConfigWith :: H.Configuration -> SiteConfig
defaultSiteConfigWith hakyllConfig =
  SiteConfig
    { _siteEnv = [],
      _siteRoot = "/",
      _siteTitle = "This Old Blog",
      _siteAuthorName = "Slurms McKenzie",
      _siteAuthorEmail = "slurms@thisold.blog",
      _siteLinkedInProfile = "https://linkedin.com/in/the-secret-ingredient",
      _siteGitWebUrl = "https://bitsof.thisold.blog/slurms/blog",
      _sitePreview = False,
      _siteDebug = False,
      _siteHakyllConfiguration = hakyllConfig,
      _siteFeedConfiguration = defaultFeedConfig,
      _siteTime = fromJust defaultTestTime,
      _siteContext = mempty
    }
