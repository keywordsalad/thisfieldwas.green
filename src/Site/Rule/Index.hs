module Site.Rule.Index (indexRules) where

import Site.Common
import Site.Context.Post
import Site.Rule.Blog (loadPublishedPosts, postCompiler, publishedSnapshot)

indexRules :: [(String, String)] -> Context String -> Rules ()
indexRules env baseCtx =
  match "index.md" do
    route $ setExtension "html"
    compile $ indexCompiler env baseCtx

indexCompiler :: [(String, String)] -> Context String -> Compiler (Item String)
indexCompiler env baseCtx = do
  let recentPosts = recentFirst =<< loadPublishedPosts
--  latestPost <- head $ fmap (take 1) recentPosts
  otherPosts <- fmap (take 4) $ fmap (drop 1) recentPosts
  let ctx = constField "title" "Home"
        <> listField "otherPosts"
          (teaserField "teaser" "content" <> baseCtx)
          (return otherPosts)
        <> postCtx
        <> baseCtx
  makeItem ""
--    >>= postCompiler env publishedSnapshot ctx
    >>= loadAndApplyTemplate "templates/index.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
