module Green.TestSupport.Config where

import Data.Time
import Green.Common
import Green.Config
import Hakyll (Configuration (..), defaultConfiguration)

defaultTestTimeString :: String
defaultTestTimeString = "2013-06-16T21:12:00-07:00"

defaultTestTime :: (MonadFail m) => m ZonedTime
defaultTestTime = timeFromString defaultTestTimeString

timeFromString :: (MonadFail m) => String -> m ZonedTime
timeFromString = parseTimeM True defaultTimeLocale "%FT%T%EZ"

defaultHakyllConfig :: Configuration
defaultHakyllConfig =
  defaultConfiguration
    { destinationDirectory = "_test/site",
      storeDirectory = "_test/store",
      tmpDirectory = "_test/tmp",
      providerDirectory = "test/data"
    }

defaultSiteConfig :: SiteConfig
defaultSiteConfig = defaultSiteConfigWith defaultHakyllConfig

defaultSiteConfigWith :: Configuration -> SiteConfig
defaultSiteConfigWith hakyllConfig =
  SiteConfig
    { _siteHakyllConfiguration = hakyllConfig,
      _siteEnv = [],
      _siteInfo =
        SiteInfo
          { _siteRoot = "/",
            _siteTitle = "This Old Blog",
            _siteDescription = "An old blog full of stuff",
            _siteAuthorName = "Old Blogger",
            _siteAuthorEmail = "blogger@thisold.blog",
            _siteLinkedInProfile = "https://linkedin.com/in/xyz1abc2def3ghi4jkl5mno6pqr7stu8vw",
            _siteTwitterProfile = "https://twitter.com/thisold.blog",
            _siteGitHubProfile = "https://github.com/thisold.blog",
            _siteGiteaProfile = "https://bitsof.thisold.blog/blogger",
            _siteGiteaWebUrl = "https://bitsof.thisold.blog/blogger/blog",
            _siteCommentsId = "blog"
          },
      _siteCurrentTime = fromJust defaultTestTime,
      _siteTimeLocale = defaultTimeLocale,
      _siteDisplayFormat =
        SiteDisplayFormat
          { _displayDateLongFormat = "%B %e, %Y %l:%M %P %EZ",
            _displayDateShortFormat = "%B %e, %Y",
            _displayTimeFormat = "%l:%M %p %EZ",
            _displayRobotDate = "%Y-%m-%d",
            _displayRobotTime = "%Y-%m-%dT%H:%M:%S%Ez",
            _displayImageWidths = [320, 768, 1024, 1920, 3840]
          },
      _siteDebug = defaultSiteDebug
    }
