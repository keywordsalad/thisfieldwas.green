module Green.Site.Robots where

import Green.Common
import Green.Config
import Green.Template
import Green.Template.Custom

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
