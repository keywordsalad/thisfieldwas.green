module Site.Rule.Robot where

import Site.Common

robotsTxtRules :: SiteConfig -> Rules ()
robotsTxtRules config = do
  match "meta/robots.txt" do
    route metaRoute
    compile $ applyAsTemplate (config ^. siteContext) =<< getResourceBody
