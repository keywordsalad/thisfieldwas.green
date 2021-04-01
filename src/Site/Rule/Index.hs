module Site.Rule.Index (indexRules) where

import Site.Common
import Site.Rule.Blog (loadPublishedPosts)

indexRules :: Context String -> Rules ()
indexRules baseCtx =
  match "index.md" do
    route $ setExtension "html"
    compile $ indexCompiler baseCtx

indexCtx :: Context String -> [Item String] -> Context String
indexCtx baseCtx posts =
  listField "posts" (postCtx <> baseCtx) (return posts)
  <> constField "title" "Home"
  <> baseCtx

indexCompiler :: Context String -> Compiler (Item String)
indexCompiler baseCtx = do
  posts <- recentFirst =<< loadPublishedPosts
  let ctx = indexCtx baseCtx posts <> baseCtx
  makeItem ""
    >>= loadAndApplyTemplate "templates/archive.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
