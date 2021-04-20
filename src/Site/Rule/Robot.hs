module Site.Rule.Robot where

import Site.Common
import Site.Util

robotsTxtRules :: Context String -> Rules ()
robotsTxtRules baseCtx =
  match "robots.txt" do
    route idRoute
    compile do
      siteRoot <- buildSiteRoot
      let robotsCtx = constField "site-root" siteRoot <> baseCtx
      applyAsTemplate robotsCtx =<< getResourceBody
