module Green.Content.Blog.Compiler where

import Data.Maybe (listToMaybe)
import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Template.Custom
import Hakyll (recentFirst)

postCompiler :: Context String -> Compiler (Item String)
postCompiler context =
  pageCompilerWithSnapshots [publishedPostsSnapshot] context =<< getResourceBody

draftCompiler :: Context String -> Compiler (Item String)
draftCompiler context =
  pageCompilerWithSnapshots [draftPostsSnapshot] context =<< getResourceBody

blogCompiler :: Context String -> Compiler (Item String)
blogCompiler context = do
  posts <- fmap (take 5) . recentFirst =<< loadPublishedPosts
  let latestPost = listToMaybe posts
      previousPosts = drop 1 posts
      context' =
        constField "latestPost" (itemValue context <$> latestPost)
          <> constField "previousPosts" (itemsValue context previousPosts)
          <> context
  pageCompiler context' =<< getResourceBody

archivesCompiler :: Context String -> Compiler (Item String)
archivesCompiler context = do
  posts <- recentFirst =<< loadPublishedPosts
  let context' = constField "posts" (itemsValue context posts) <> context
  pageCompiler context' =<< getResourceBody

draftArchivesCompiler :: Context String -> Compiler (Item String)
draftArchivesCompiler context = do
  drafts <- recentFirst =<< loadDraftPosts
  let context' = constField "drafts" (itemsValue context drafts) <> context
  pageCompiler context' =<< getResourceBody

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots "_drafts/**" draftPostsSnapshot

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = loadExistingSnapshots "_posts/**" publishedPostsSnapshot

publishedPostsSnapshot :: String
publishedPostsSnapshot = "_publishedPosts"

draftPostsSnapshot :: String
draftPostsSnapshot = "_draftPosts"
