module Green.Content.Blog
  ( blog,
    loadPublishedPosts,
    publishedPostsSnapshot,
    recentPostsContext,
  )
where

import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Route
import Green.Template
import Green.Template.Custom.Compiler (pageCompiler, pageCompilerWithSnapshots)
import qualified Hakyll as H

blog :: Context String -> Rules ()
blog context = do
  tags <- buildTags "_posts/**" makeTagId
  tagsIndex tags context
  blogIndex tags context
  archives context
  draftArchives context
  posts context
  drafts context

tagsIndex :: Tags -> Context String -> Rules ()
tagsIndex tags context = do
  match "blog/tag.html" do
    compile getResourceString
  H.tagsRules tags \tag pat -> do
    route indexRoute
    compile do
      tagPosts <- H.recentFirst =<< H.loadAll pat
      let tagsContext =
            constField "tag" tag
              <> constField "title" ("Posts tagged \"" ++ tag ++ "\"")
              <> constField "posts" (itemListValue context tagPosts)
              <> constField "layout" ("page" :: String)
              <> context
      H.loadBody (fromFilePath "blog/tag.html")
        >>= makeItem
        >>= pageCompiler tagsContext
        >>= relativizeUrls

blogIndex :: Tags -> Context String -> Rules ()
blogIndex tags context =
  match "blog/index.html" do
    route idRoute
    compile do
      tagCloud <- renderTagCloud 0.8 2.0 tags
      recentPosts <- recentPostsContext
      let blogContext =
            constField "tagCloud" tagCloud
              <> recentPosts
              <> context
      getResourceBody
        >>= pageCompiler blogContext
        >>= relativizeUrls

archives :: Context String -> Rules ()
archives context = do
  match "blog/archives.html" do
    route indexRoute
    compile do
      publishedPosts <- H.recentFirst =<< loadPublishedPosts
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
      draftPosts <- H.recentFirst =<< loadDraftPosts
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
        >>= pageCompilerWithSnapshots [publishedPostsSnapshot] postContext
        >>= relativizeUrls
  where
    postContext =
      tagsField "tags"
        <> context

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
  recentPosts <- fmap (take 5) . H.recentFirst =<< loadPublishedPosts
  let latestPost = take 1 recentPosts
      previousPosts = drop 1 recentPosts
  return $
    constField "latestPost" (itemListValue teaserContext latestPost)
      <> constField "previousPosts" (itemListValue teaserContext previousPosts)

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
dateRoute = gsubRoute datePattern (H.replaceAll "-" (const "/"))
