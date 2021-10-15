module Green.Content.Blog.Compiler where

import Data.Maybe (listToMaybe)
import Green.Common
import qualified Green.Common as H
import Green.Compiler (loadExistingSnapshots)
import Green.Template.Custom
import qualified Hakyll as H

postCompiler :: Context String -> Compiler (Item String)
postCompiler context =
  pageCompilerWithSnapshots [publishedPostsSnapshot] context =<< H.getResourceBody

draftCompiler :: Context String -> Compiler (Item String)
draftCompiler context =
  pageCompilerWithSnapshots [draftPostsSnapshot] context =<< H.getResourceBody

blogCompiler :: Context String -> Compiler (Item String)
blogCompiler context = do
  posts <- fmap (take 5) . H.recentFirst =<< loadPublishedPosts
  let latestPost = listToMaybe posts
      previousPosts = drop 1 posts
      context' =
        constField "latestPost" latestPost
          <> constField "previousPosts" previousPosts
          <> context
  pageCompiler context' =<< H.getResourceBody

archivesCompiler :: Context String -> Compiler (Item String)
archivesCompiler context = do
  posts <- H.recentFirst =<< loadPublishedPosts
  let context' = constField "posts" posts <> context
  pageCompiler context' =<< H.getResourceBody

draftArchivesCompiler :: Context String -> Compiler (Item String)
draftArchivesCompiler context = do
  drafts <- H.recentFirst =<< loadDraftPosts
  let context' = constField "drafts" drafts <> context
  pageCompiler context' =<< H.getResourceBody

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots "_drafts/**" draftPostsSnapshot

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = loadExistingSnapshots "_posts/**" publishedPostsSnapshot

publishedPostsSnapshot :: String
publishedPostsSnapshot = "_publishedPosts"

draftPostsSnapshot :: String
draftPostsSnapshot = "_draftPosts"
