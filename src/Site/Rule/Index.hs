module Site.Rule.Index (indexRules) where

import Hakyll
import Site.Context.Post
import Site.Rule.Blog (loadPublishedPosts, contentOnly, publishedSnapshot)

indexRules :: [(String, String)] -> Context String -> Rules ()
indexRules env baseCtx =
  match "index.html" do
    route idRoute
    compile $ indexCompiler env baseCtx

indexCompiler :: [(String, String)] -> Context String -> Compiler (Item String)
indexCompiler _env baseCtx = do
  recentPosts <- take 5 <$> (recentFirst =<< loadPublishedPosts)
  let ctx = listField "recentPosts" (teaserCtx <> baseCtx) (return recentPosts)
          <> postCtx
          <> baseCtx
  getResourceBody
    >>= applyAsTemplate ctx
    >>= loadAndApplyTemplate "templates/page.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
  where
    teaserCtx = teaserField "teaser" (contentOnly publishedSnapshot)
