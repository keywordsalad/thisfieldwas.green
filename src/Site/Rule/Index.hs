module Site.Rule.Index (indexRules) where

import Hakyll
import Site.Common
import Site.Context.Post
import Site.Rule.Blog (loadPublishedPosts)

indexRules :: [(String, String)] -> Context String -> Rules ()
indexRules env baseCtx =
  match "index.md" do
    route $ setExtension "html"
    compile $ indexCompiler env baseCtx

indexCompiler :: [(String, String)] -> Context String -> Compiler (Item String)
indexCompiler _env baseCtx = do
  recentPosts <- take 5 <$> (recentFirst =<< loadPublishedPosts)
  let teasers = mapContext demoteHeaders (teaserField "teaser" "content")
  let ctx =
        -- page
        constField "siteTitle" "Software Engineering and Me"
          <> listField "recentPosts" (teasers <> baseCtx) (return recentPosts)
          <> postCtx
          <> baseCtx

  makeItem ""
    >>= loadAndApplyTemplate "templates/index.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
