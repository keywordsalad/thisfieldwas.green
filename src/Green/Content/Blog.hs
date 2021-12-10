module Green.Content.Blog
  ( blog,
    loadPublishedPosts,
    publishedPostsSnapshot,
    recentPostsContext,
  )
where

import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Config
import Green.Route
import Green.Template
import Green.Template.Custom
import qualified Hakyll as H

blog :: SiteConfig -> Context String -> Rules ()
blog config context = do
  let postsPattern = config ^. sitePostsPattern
  categories <- buildCategories postsPattern makeCategoryId
  tags <- buildTags postsPattern makeTagId

  blogHome config categories tags context
  posts postsPattern context
  archives config context

  categoriesPages categories context
  tagsPages tags context

  draftsIndex context
  drafts config context

blogHome :: SiteConfig -> Tags -> Tags -> Context String -> Rules ()
blogHome config categories tags context =
  match "blog.html" do
    route indexRoute
    compile do
      categoryCloud <- renderTagCloud categories
      tagCloud <- renderTagCloud tags
      recentPosts <- recentPostsContext config
      let blogContext =
            constField "categoryCloud" categoryCloud
              <> constField "tagCloud" tagCloud
              <> recentPosts
              <> postContext
              <> context
      getResourceBody
        >>= contentCompiler blogContext
        >>= layoutCompiler blogContext
        >>= relativizeUrls

archives :: SiteConfig -> Context String -> Rules ()
archives config context = do
  match "archives.html" do
    route indexRoute
    compile do
      publishedPosts <- H.recentFirst =<< loadPublishedPosts config
      let archivesContext =
            constField "posts" (itemListValue context publishedPosts)
              <> postContext
              <> context
      getResourceBody
        >>= contentCompiler archivesContext
        >>= layoutCompiler archivesContext
        >>= relativizeUrls

draftsIndex :: Context String -> Rules ()
draftsIndex context = do
  match "drafts.html" do
    route indexRoute
    compile do
      draftPosts <- H.recentFirst =<< loadDraftPosts
      let draftsContext =
            constField "posts" (itemListValue context draftPosts)
              <> postContext
              <> context
      getResourceBody
        >>= contentCompiler draftsContext
        >>= layoutCompiler draftsContext
        >>= relativizeUrls

posts :: Pattern -> Context String -> Rules ()
posts postsPattern context = do
  match postsPattern do
    route $
      subPrefixRoute "_posts/" "blog/"
        `composeRoutes` dateRoute
        `composeRoutes` setExtension "html"
        `composeRoutes` indexRoute
    compile $
      getResourceBody
        >>= contentCompiler postsContext
        >>= snapshotCompiler [publishedPostsSnapshot]
        >>= layoutCompiler postsContext
        >>= relativizeUrls
  where
    postsContext = postContext <> context

drafts :: SiteConfig -> Context String -> Rules ()
drafts config context = do
  match "_drafts/**" do
    route $
      subPrefixRoute "_drafts/" "drafts/"
        `composeRoutes` dateRoute
        `composeRoutes` setExtension "html"
        `composeRoutes` indexRoute
    compile $
      getResourceBody
        >>= contentCompiler draftsContext
        >>= snapshotCompiler snapshots
        >>= layoutCompiler draftsContext
        >>= relativizeUrls
  where
    draftsContext = postContext <> context
    snapshots =
      draftPostsSnapshot :
        [publishedPostsSnapshot | config ^. siteDebug . debugPreview]

categoriesPages :: Tags -> Context String -> Rules ()
categoriesPages categories context =
  H.tagsRules categories \category pat -> do
    route indexRoute
    compile do
      categoryPosts <- H.recentFirst =<< H.loadAll pat
      let categoryContext =
            constField "category" category
              <> constField "title" ("Posts under \"" ++ category ++ "\"")
              <> constField "posts" (itemListValue context categoryPosts)
              <> constField "layout" ("page" :: String)
              <> postContext
              <> context
      dummy <- makeItem ""
      template <- loadBody "_templates/posts-under-category.html"
      applyTemplate template categoryContext dummy
        >>= pandocCompiler
        >>= layoutCompiler categoryContext
        >>= relativizeUrls

tagsPages :: Tags -> Context String -> Rules ()
tagsPages tags context =
  H.tagsRules tags \tag pat -> do
    route indexRoute
    compile do
      tagPosts <- H.recentFirst =<< H.loadAll pat
      let tagsContext =
            constField "tag" tag
              <> constField "title" ("Posts tagged \"" ++ tag ++ "\"")
              <> constField "posts" (itemListValue context tagPosts)
              <> constField "layout" ("page" :: String)
              <> postContext
              <> context
      dummy <- makeItem ""
      template <- loadBody "_templates/posts-under-tag.html"
      applyTemplate template tagsContext dummy
        >>= pandocCompiler
        >>= layoutCompiler tagsContext
        >>= relativizeUrls

postContext :: Context String
postContext =
  categoryLinksField "categoryLinks"
    <> tagLinksField "tagLinks"
    <> postHeaderField "postHeader"

recentPostsContext :: SiteConfig -> Compiler (Context String)
recentPostsContext config = do
  recentPosts <- fmap (take 5) . H.recentFirst =<< loadPublishedPosts config
  let latestPost = take 1 recentPosts
      previousPosts = drop 1 recentPosts
  return $
    constField "latestPost" (itemListValue teaserContext latestPost)
      <> constField "previousPosts" (itemListValue teaserContext previousPosts)

teaserContext :: Context String
teaserContext = teaserField "teaser" publishedPostsSnapshot

loadPublishedPosts :: SiteConfig -> Compiler [Item String]
loadPublishedPosts config = loadExistingSnapshots (config ^. sitePostsPattern) publishedPostsSnapshot

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

postHeaderField :: String -> Context String
postHeaderField key = functionField key f
  where
    defaults = defaultKeys ["headerLevel", "latestPost"]
    f (fields :: Context String) = do
      tplWithContext (fields <> defaults) do
        itemBody <$> loadAndApplyTemplate' (fromFilePath "_templates/post-header.html")
