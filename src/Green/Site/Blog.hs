module Green.Site.Blog where

import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Config
import Green.Route
import Green.Template
import Green.Template.Custom
import qualified Hakyll as H
import System.FilePath

buildBlogCategories :: (H.MonadMetadata m) => Bool -> m Tags
buildBlogCategories preview = buildCategories (postsPattern preview) makeCategoryId

buildBlogTags :: (H.MonadMetadata m) => Bool -> m Tags
buildBlogTags preview = buildTags (postsPattern preview) makeTagId

blog :: SiteConfig -> Context String -> Rules ()
blog config context = do
  let preview = config ^. sitePreview
  categories <- buildBlogCategories preview
  tags <- buildBlogTags preview

  blogHome preview tags categories context
  posts context
  previews config context
  archives preview context

  tagsPages tags context
  categoriesPages categories context

  draftsIndex context
  drafts context

blogHome :: Bool -> Tags -> Tags -> Context String -> Rules ()
blogHome preview tags categories context =
  match "blog.html" do
    route indexRoute
    compile do
      categoryCloud <- renderTagCloud categories
      tagCloud <- renderTagCloud tags
      recentPosts <- recentPostsContext preview
      getResourceBody >>= applyTemplates do
        applyContext $
          constField "tagCloud" tagCloud
            <> constField "categoryCloud" categoryCloud
            <> recentPosts
            <> postContext
            <> context
        applyContent
        applyLayout

archives :: Bool -> Context String -> Rules ()
archives preview context = do
  match "archives.html" do
    route indexRoute
    compile do
      publishedPosts <- H.recentFirst =<< loadPublishedPosts preview
      getResourceBody >>= applyTemplates do
        applyContext $
          itemsField "posts" postContext publishedPosts
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
          itemsField "posts" postContext draftPosts
            <> constField "noindex" True
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
        saveSnapshots ["content"]
        applyLayout

postsRoute :: Routes
postsRoute =
  subRoute "^_posts/" "blog/"
    `composeRoutes` dateRoute
    `composeRoutes` setExtension "html"
    `composeRoutes` indexRoute

previews :: SiteConfig -> Context String -> Rules ()
previews config context
  | config ^. sitePreview = do
    match "_drafts/**" $ version "preview" do
      route $ previewRoute (config ^. siteTimeLocale)
      compile $
        getResourceBody >>= applyTemplates do
          applyContext $ postContext <> context
          applyContent
          saveSnapshots ["content"]
          applyLayout
  | otherwise = return ()

previewRoute :: TimeLocale -> Routes
previewRoute timeLocale =
  subRoute "^_drafts/" "blog/"
    `composeRoutes` metadataRoute
      ( \m ->
          customRoute
            ( \id' ->
                let dirName = takeDirectory $ toFilePath id'
                    baseName = takeBaseName $ toFilePath id'
                    date =
                      formatTime timeLocale "%Y/%m/%d" . fromJust $
                        dateFromMetadata timeLocale ["published", "date"] m
                          >>= parseTimeM' timeLocale normalizedFormat
                 in dirName </> date </> baseName
            )
      )
    `composeRoutes` setExtension "html"
    `composeRoutes` indexRoute

drafts :: Context String -> Rules ()
drafts context = do
  match "_drafts/**" do
    route draftsRoute
    compile $
      getResourceBody >>= applyTemplates do
        applyContext $
          postContext
            <> constField "noindex" True
            <> context
        applyContent
        saveSnapshots ["content"]
        applyLayout

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
            <> itemsField "posts" postContext categoryPosts
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
            <> itemsField "posts" postContext tagPosts
            <> constField "layout" ("page" :: String)
            <> context
        applyTemplate "_templates/posts-under-tag.html"
        applyCompiler pandocCompiler
        applyLayout

postContext :: Context String
postContext =
  categoryLinksField "categoryLinks"
    <> tagLinksField "tagLinks"
    <> tagListField "tagList"
    <> categoryListField "categoryList"
    <> constField "article" True
    <> namedMetadataField "title"

recentPostsContext :: Bool -> Compiler (Context String)
recentPostsContext preview = do
  recentPosts <- fmap (take 5) . H.recentFirst =<< loadPublishedPosts preview
  let latestPost = take 1 recentPosts
      previousPosts = drop 1 recentPosts
  return $
    itemsField "latestPost" teaserContext latestPost
      <> itemsField "previousPosts" teaserContext previousPosts

teaserContext :: Context String
teaserContext = teaserField "teaser" "content"

loadPublishedPosts :: Bool -> Compiler [Item String]
loadPublishedPosts preview = loadExistingSnapshots (postsPattern preview) "content"

postsPattern :: Bool -> Pattern
postsPattern = \case
  True -> "_posts/**" .||. "_drafts/**"
  False -> "_posts/**"

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots ("_drafts/**" .&&. hasNoVersion) "content"

dateRoute :: Routes
dateRoute = gsubRoute datePattern (H.replaceAll "-" (const "/"))
  where
    datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"

tagListField :: String -> Context String
tagListField key = field key $ lift . getTags . itemIdentifier

categoryListField :: String -> Context String
categoryListField key = field key $ lift . getCategory . itemIdentifier
