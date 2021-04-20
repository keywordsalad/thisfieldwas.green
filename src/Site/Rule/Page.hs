module Site.Rule.Page (pageRules) where

import Site.Common

pageRules :: SiteConfig -> Rules ()
pageRules config = do
  matchMetadata "pages/**" isStaticPage do
    route htmlPageRoute
    compile $
      interpolateResourceBody config
        >>= applyLayoutFromMetadata config
        >>= relativizeUrls
