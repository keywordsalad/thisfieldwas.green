module Green.Site.Blog where

import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Route
import Green.Template
import Green.Template.Custom
import qualified Hakyll as H

buildBlogCategories :: (H.MonadMetadata m) => m Tags
buildBlogCategories = buildCategories "_posts/**" makeCategoryId

buildBlogTags :: (H.MonadMetadata m) => m Tags
buildBlogTags = buildTags "_posts/**" makeTagId

blog :: Context String -> Rules ()
blog context = do
  categories <- buildBlogCategories
  tags <- buildBlogTags

  blogHome categories tags context
  posts context
  archives context

  categoriesPages categories context
  tagsPages tags context

  draftsIndex context
  drafts context

blogHome :: Tags -> Tags -> Context String -> Rules ()
blogHome categories tags context =
  match "blog.html" do
    route indexRoute
    compile do
      categoryCloud <- renderTagCloud categories
      tagCloud <- renderTagCloud tags
      recentPosts <- recentPostsContext
      getResourceBody >>= applyTemplates do
        applyContext $
          constField "categoryCloud" categoryCloud
            <> constField "tagCloud" tagCloud
            <> recentPosts
            <> postContext
            <> context
        applyContent
        applyLayout

archives :: Context String -> Rules ()
archives context = do
  match "archives.html" do
    route indexRoute
    compile do
      publishedPosts <- H.recentFirst =<< loadPublishedPosts
      getResourceBody >>= applyTemplates do
        applyContext $
          constField "posts" (itemListValue (postContext <> context) publishedPosts)
            <> context
        applyContent
        applyLayout

draftsIndex :: Context String -> Rules ()
draftsIndex context = do
  match "drafts.html" do
    route indexRoute
    compile do
      draftPosts <- H.recentFirst =<< loadDraftPosts
      getResourceBody >>= applyTemplates do
        applyContext $
          constField "posts" (itemListValue (postContext <> context) draftPosts)
            <> context
        applyContent
        applyLayout

posts :: Context String -> Rules ()
posts context = do
  match "_posts/**" do
    route postsRoute
    compile $
      getResourceBody >>= applyTemplates do
        applyContext $ postContext <> context
        applyContent
        saveSnapshots [publishedPostsSnapshot]
        applyLayout

postsRoute :: Routes
postsRoute =
  subRoute "^_posts/" "blog/"
    `composeRoutes` dateRoute
    `composeRoutes` setExtension "html"
    `composeRoutes` indexRoute

drafts :: Context String -> Rules ()
drafts context = do
  match "_drafts/**" do
    route draftsRoute
    compile $
      getResourceBody >>= applyTemplates do
        applyContext draftsContext
        applyContent
        saveSnapshots [draftPostsSnapshot]
        applyLayout
  where
    draftsContext = postContext <> context

draftsRoute :: Routes
draftsRoute =
  subRoute "_drafts/" "drafts/"
    `composeRoutes` dateRoute
    `composeRoutes` setExtension "html"
    `composeRoutes` indexRoute

categoriesPages :: Tags -> Context String -> Rules ()
categoriesPages categories context =
  H.tagsRules categories \category pat -> do
    route indexRoute
    compile do
      categoryPosts <- H.recentFirst =<< H.loadAll pat
      makeItem "" >>= applyTemplates do
        applyContext $
          constField "category" category
            <> constField "title" ("Posts under \"" ++ category ++ "\"")
            <> constField "posts" (itemListValue (postContext <> context) categoryPosts)
            <> constField "layout" ("page" :: String)
            <> context
        applyTemplate "_templates/posts-under-category.html"
        applyCompiler pandocCompiler
        applyLayout

tagsPages :: Tags -> Context String -> Rules ()
tagsPages tags context =
  H.tagsRules tags \tag pat -> do
    route indexRoute
    compile do
      tagPosts <- H.recentFirst =<< H.loadAll pat
      makeItem "" >>= applyTemplates do
        applyContext $
          constField "tag" tag
            <> constField "title" ("Posts tagged \"" ++ tag ++ "\"")
            <> constField "posts" (itemListValue (postContext <> context) tagPosts)
            <> constField "layout" ("page" :: String)
            <> context
        applyTemplate "_templates/posts-under-tag.html"
        applyCompiler pandocCompiler
        applyLayout

postContext :: Context String
postContext =
  categoryLinksField "categoryLinks"
    <> tagLinksField "tagLinks"

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

dateRoute :: Routes
dateRoute = gsubRoute datePattern (H.replaceAll "-" (const "/"))
  where
    datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"
