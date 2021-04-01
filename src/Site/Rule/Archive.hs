module Site.Rule.Archive (archiveRules) where

import Site.Common
import Site.Rule.Blog (loadPublishedPosts)

archiveRules :: Context String -> Rules ()
archiveRules baseCtx =
  create ["archive.html"] do
    route idRoute
    compile $ archiveCompiler baseCtx

archiveCtx :: Context String -> [Item String] -> Context String
archiveCtx baseCtx posts =
  listField "posts" (postCtx <> baseCtx) (return posts)
  <> constField "title" "Archives"

archiveCompiler :: Context String -> Compiler (Item String)
archiveCompiler baseCtx = do
  posts <- recentFirst =<< loadPublishedPosts
  let ctx = archiveCtx baseCtx posts <> baseCtx
  makeItem ""
    >>= loadAndApplyTemplate "templates/archive.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
