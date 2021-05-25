module Site.Rule.Page (pageRules) where

import Site.Common

pageRules :: SiteConfig -> Rules ()
pageRules config = do
  match "pages/**" do
    route pageRoute
    compile $
      interpolateResourceBody config
        >>= applyLayoutFromMetadata config
        >>= relativizeUrls
