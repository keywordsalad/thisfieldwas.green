module Green.Site.Blog where

import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Config
import Green.Route
import Green.Template
import Green.Template.Custom
import qualified Hakyll as H
import System.FilePath

buildBlogTags :: (H.MonadMetadata m) => m Tags
buildBlogTags = buildTags postsPattern makeTagId

blog :: SiteConfig -> Context String -> Rules ()
blog config context = do
  tags <- buildBlogTags
  blogHome tags context
  posts context
  previews config context
  archives context
  tagsPages tags context
  draftsIndex context
  drafts context

blogHome :: Tags -> Context String -> Rules ()
blogHome tags context =
  match "blog.html" do
    route indexRoute
    compile do
      tagCloud <- renderTagCloud tags
      recentPosts <- recentPostsContext
      getResourceBody >>= applyTemplates do
        applyContext $
          constField "tagCloud" tagCloud
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
  tagLinksField "tagLinks"
    <> tagListField "tagList"
    <> constField "article" True
    <> namedMetadataField "title"

recentPostsContext :: Compiler (Context String)
recentPostsContext = do
  recentPosts <- fmap (take 5) . H.recentFirst =<< loadPublishedPosts
  let latestPost = take 1 recentPosts
      previousPosts = drop 1 recentPosts
  return $
    itemsField "latestPost" teaserContext latestPost
      <> itemsField "previousPosts" teaserContext previousPosts

teaserContext :: Context String
teaserContext = teaserField "teaser" "content"

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = loadExistingSnapshots postsPattern "content"

postsPattern :: Pattern
postsPattern = "_posts/**" .||. ("_drafts/**" .&&. hasVersion "preview")

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots ("_drafts/**" .&&. hasNoVersion) "content"

dateRoute :: Routes
dateRoute = gsubRoute datePattern (H.replaceAll "-" (const "/"))
  where
    datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"

tagListField :: String -> Context String
tagListField key = field key $ lift . getTags . itemIdentifier
