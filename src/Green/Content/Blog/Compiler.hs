module Green.Content.Blog.Compiler where

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
  blogContext <- (<> context) <$> recentPostsContext
  pageCompiler blogContext =<< getResourceBody

archivesCompiler :: Context String -> Compiler (Item String)
archivesCompiler context = do
  posts <- recentFirst =<< loadPublishedPosts
  let archivesContext =
        constField "posts" (itemListValue context posts)
          <> context
  pageCompiler archivesContext =<< getResourceBody

draftArchivesCompiler :: Context String -> Compiler (Item String)
draftArchivesCompiler context = do
  drafts <- recentFirst =<< loadDraftPosts
  let draftsContext =
        constField "posts" (itemListValue context drafts)
          <> context
  pageCompiler draftsContext =<< getResourceBody

recentPostsContext :: Compiler (Context String)
recentPostsContext =
  withErrorMessage "Failed to load recent posts context" do
    posts <- fmap (take 5) . recentFirst =<< loadPublishedPosts
    let latestPost = take 1 posts
        previousPosts = drop 1 posts
        blogContext =
          constField "latestPost" (itemListValue teaserContext latestPost)
            <> constField "previousPosts" (itemListValue teaserContext previousPosts)
    return blogContext

teaserContext :: Context String
teaserContext = teaserField "teaser" publishedPostsSnapshot

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = withErrorMessage "Failed to load published posts" $ do
  snapshots <- loadExistingSnapshots "_posts/**" publishedPostsSnapshot
  debugCompiler $ "Loaded " ++ show (length snapshots) ++ " published posts"
  return snapshots

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots "_drafts/**" draftPostsSnapshot

publishedPostsSnapshot :: String
publishedPostsSnapshot = "_publishedPosts"

draftPostsSnapshot :: String
draftPostsSnapshot = "_draftPosts"
