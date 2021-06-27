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
  publishedPosts <- recentFirst =<< loadPublishedPosts
  let recentPosts = take 5 . drop 1 $ publishedPosts
  let latestPost = head . take 1 $ publishedPosts
  let latestPostId = itemIdentifier latestPost
  latestPostTitle <- fromJust <$> getMetadataField latestPostId "title"
  latestPostUrl <- toUrl . fromJust <$> getRoute latestPostId

  let teasers = mapContext demoteHeaders (teaserField "teaser" "content")
  let ctx =
        -- page
        constField "siteTitle" "Software Engineering and Me"
          -- latest post
          <> constField "title" latestPostTitle
          <> constField "latestPostTitle" latestPostTitle
          <> constField "latestPostUrl" latestPostUrl
          <> constField "latestPost" (itemBody latestPost)
          -- recent posts
          <> listField "recentPosts" (teasers <> baseCtx) (return recentPosts)
          -- rest of context
          <> postCtx
          <> baseCtx

  makeItem ""
    >>= loadAndApplyTemplate "templates/index.html" ctx
    >>= loadAndApplyTemplate "templates/default.html" ctx
    >>= relativizeUrls
