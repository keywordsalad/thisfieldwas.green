module Green.Site.Robots where

import Green.Common
import Green.Config
import Green.Hakyllbars as HB

robotsTxt :: SiteConfig -> Context String -> Rules ()
robotsTxt config context =
  match robots do
    route $ constRoute "robots.txt"
    compile $
      getResourceBody >>= applyTemplates do
        applyContext context
        applyAsTemplate
  where
    robots
      | config ^. sitePreview = "robots-preview.txt"
      | otherwise = "robots.txt"
