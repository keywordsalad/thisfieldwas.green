module Green.Rule.Index where

import Green.Common
import Green.Compiler
import Green.Config
import Green.Lens
import Green.Rule.Blog

indexRules :: SiteConfig -> Rules ()
indexRules config =
  match "index.html" do
    route idRoute
    compile $ indexCompiler config

indexCompiler :: SiteConfig -> Compiler (Item String)
indexCompiler config = do
  recentPosts <- take 5 <$> (recentFirst =<< loadPostsContent)
  let localConfig = config & siteContext ~<> recentPostsField recentPosts
  interpolateResourceBody localConfig
    >>= applyLayout localConfig
    >>= relativizeUrls
  where
    teaserCtx = teaserField "teaser" (contentOnly postSnapshot)
    recentPostsField = listField "recentPosts" (teaserCtx <> (config ^. siteContext)) . return
