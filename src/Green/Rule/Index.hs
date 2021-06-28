module Green.Rule.Index where

import Green.Common
import Green.Rule.Blog

indexRules :: SiteConfig -> Rules ()
indexRules config =
  match "index.html" do
    route idRoute
    compile $ indexCompiler config

indexCompiler :: SiteConfig -> Compiler (Item String)
indexCompiler config = do
  recentPosts <- take 5 <$> (recentFirst =<< loadPosts)
  let ctx =
        listField
          "recentPosts"
          (teaserCtx <> (config ^. siteContext))
          (return recentPosts)
          <> postContext
          <> (config ^. siteContext)
  getResourceBody
    >>= applyAsTemplate ctx
    >>= applyLayoutFromMetadata config
    >>= relativizeUrls
  where
    teaserCtx = teaserField "teaser" (contentOnly postSnapshot)
