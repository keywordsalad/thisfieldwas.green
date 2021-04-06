module Site.Rule.Index (indexRules) where

import Site.Common
import Site.Context.Post
import Site.Rule.Blog (loadPublishedPosts)

indexRules :: [(String, String)] -> Context String -> Rules ()
indexRules env baseCtx =
  match "index.html" do
    route $ setExtension "html"
    compile $ indexCompiler env baseCtx

indexCompiler :: [(String, String)] -> Context String -> Compiler (Item String)
indexCompiler env baseCtx = do
  let recentPosts = recentFirst =<< loadPublishedPosts

  -- the most recent post
  latestPost <- head . take 1 <$> recentPosts
  let latestPostId = itemIdentifier latestPost
  latestPostTitle <- fromJust <$> getMetadataField latestPostId "title"
  latestPostUrl <- toUrl . fromJust <$> getRoute latestPostId

  -- other recent posts
  otherPosts <- take 5 . drop 1 <$> recentPosts

  let ctx =
        -- latest post
        constField "latestPostTitle" latestPostTitle
          <> constField "latestPostUrl" latestPostUrl
          <> constField "latestPost" (itemBody latestPost)
          -- recent posts
          <> listField
            "otherPosts"
            (teaserField "teaser" "content" <> baseCtx)
            (return otherPosts)
          -- rest of context
          <> postCtx
          <> baseCtx

  interpolateResourceBody env ctx
    >>= applySkeletonTemplate ctx
    >>= relativizeUrls
