module Green.Content.Blog (blog, recentPostsContext) where

import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Route
import Green.Template
import Green.Template.Custom.Compiler (pageCompiler, pageCompilerWithSnapshots)
import Hakyll (recentFirst, replaceAll)

blog :: Context String -> Rules ()
blog context =
  let rules =
        [ blogIndex,
          archives,
          draftArchives,
          posts,
          drafts
        ]
   in sequenceA_ $ rules <*> pure context

blogIndex :: Context String -> Rules ()
blogIndex context =
  match "blog/index.html" do
    route idRoute
    compile do
      blogContext <- (<> context) <$> recentPostsContext
      getResourceBody
        >>= pageCompiler blogContext
        >>= relativizeUrls

archives :: Context String -> Rules ()
archives context = do
  match "blog/archives.html" do
    route indexRoute
    compile do
      publishedPosts <- recentFirst =<< loadPublishedPosts
      let archivesContext =
            constField "posts" (itemListValue context publishedPosts)
              <> context
      getResourceBody
        >>= pageCompiler archivesContext
        >>= relativizeUrls

draftArchives :: Context String -> Rules ()
draftArchives context = do
  match "blog/drafts.html" do
    route indexRoute
    compile do
      draftPosts <- recentFirst =<< loadDraftPosts
      let draftsContext =
            constField "posts" (itemListValue context draftPosts)
              <> context
      getResourceBody
        >>= pageCompiler draftsContext
        >>= relativizeUrls

posts :: Context String -> Rules ()
posts context = do
  match "_posts/**" do
    route $
      subPrefixRoute "_posts/" "blog/"
        `composeRoutes` dateRoute
        `composeRoutes` setExtension "html"
        `composeRoutes` indexRoute
    compile $
      getResourceBody
        >>= pageCompilerWithSnapshots [publishedPostsSnapshot] context
        >>= relativizeUrls

drafts :: Context String -> Rules ()
drafts context = do
  match "_drafts/**" do
    route $
      subPrefixRoute "_drafts/" "blog/drafts/"
        `composeRoutes` dateRoute
        `composeRoutes` setExtension "html"
        `composeRoutes` indexRoute
    compile $
      getResourceBody
        >>= pageCompilerWithSnapshots [draftPostsSnapshot] context
        >>= relativizeUrls

recentPostsContext :: Compiler (Context String)
recentPostsContext = do
  recentPosts <- fmap (take 5) . recentFirst =<< loadPublishedPosts
  let latestPost = take 1 recentPosts
      previousPosts = drop 1 recentPosts
      blogContext =
        constField "latestPost" (itemListValue teaserContext latestPost)
          <> constField "previousPosts" (itemListValue teaserContext previousPosts)
  return blogContext

teaserContext :: Context String
teaserContext = teaserField "teaser" publishedPostsSnapshot

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = loadExistingSnapshots "_posts/**" publishedPostsSnapshot

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots "_drafts/**" draftPostsSnapshot

publishedPostsSnapshot :: String
publishedPostsSnapshot = "_publishedPosts"

draftPostsSnapshot :: String
draftPostsSnapshot = "_draftPosts"

datePattern :: String
datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"

dateRoute :: Routes
dateRoute = gsubRoute datePattern (replaceAll "-" (const "/"))
