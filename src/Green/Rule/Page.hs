module Green.Rule.Page (pageRules) where

import Green.Common

pageRules :: SiteConfig -> Rules ()
pageRules config = do
  match "pages/**" do
    route pageRoute
    compile $
      interpolateResourceBody config
        >>= applyLayoutFromMetadata config
        >>= relativizeUrls
