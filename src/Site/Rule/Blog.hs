module Site.Rule.Blog
  ( blogRules,
    draftSnapshot,
    loadDraftPosts,
    loadPublishedPosts,
    postCompiler,
    postCtx,
    publishedSnapshot,
  )
where

import Site.Common
import Site.Context.Post
import Site.Route (indexRoute)

type PostSnapshot = String

publishedSnapshot :: PostSnapshot
publishedSnapshot = "_publishedposts"

draftSnapshot :: PostSnapshot
draftSnapshot = "_draftposts"

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = loadExistingSnapshots "blog/*" publishedSnapshot

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots "blog/*" draftSnapshot

blogRules :: [(String, String)] -> Context String -> Rules ()
blogRules env baseCtx = do
  publishedPostRules env baseCtx
  draftPostRules env baseCtx
  draftIndexRules baseCtx

publishedPostRules :: [(String, String)] -> Context String -> Rules ()
publishedPostRules env baseCtx =
  matchMetadata "blog/*" (isPublishable env) do
    route baseRoute
    compile $
      postCompiler env publishedSnapshot ctx
        >>= saveSnapshot "content"
        >>= applyPageTemplates ctx
        >>= relativizeUrls
  where
    ctx = postCtx <> baseCtx

draftPostRules :: [(String, String)] -> Context String -> Rules ()
draftPostRules env baseCtx =
  matchMetadata "blog/*" isDraft do
    route $ baseRoute `composeRoutes` draftsRoute
    compile $
      postCompiler env draftSnapshot ctx
        >>= applyPageTemplates ctx
        >>= relativizeUrls
  where
    ctx = postCtx <> baseCtx
    draftsRoute = gsubRoute "^blog/" $ replaceAll "^blog/" (const "drafts/")

postCompiler ::
  [(String, String)] ->
  Snapshot ->
  Context String ->
  Compiler (Item String)
postCompiler env snapshot ctx = do
  interpolateResourceBody env ctx
    >>= applyContentTemplates ctx
    >>= saveSnapshot snapshot

baseRoute :: Routes
baseRoute =
  setExtension "html"
    `composeRoutes` indexRoute
    `composeRoutes` dateRoute

dateRoute :: Routes
dateRoute = gsubRoute "^blog/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")

draftIndexRules :: Context String -> Rules ()
draftIndexRules baseCtx =
  create ["drafts.html"] do
    route $ idRoute `composeRoutes` indexRoute
    compile $ draftIndexCompiler baseCtx

draftIndexCtx :: Context String -> [Item String] -> Context String
draftIndexCtx baseCtx posts =
  constField "title" "Drafts"
    <> listField "posts" (postCtx <> baseCtx) (return posts)
    <> constField "title" "Drafts"

draftIndexCompiler :: Context String -> Compiler (Item String)
draftIndexCompiler baseCtx = do
  posts <- recentFirst =<< loadDraftPosts
  let ctx = draftIndexCtx baseCtx posts <> baseCtx
  makeItem ""
    >>= loadAndApplyTemplate "templates/drafts.html" ctx
    >>= applyPageTemplates ctx
    >>= relativizeUrls

isPublishable :: [(String, String)] -> Metadata -> Bool
isPublishable env metadata = or $ [isPreview env, isPublished] <*> [metadata]

isPublished :: Metadata -> Bool
isPublished = maybe True (== "true") . lookupString "published"

isPreview :: [(String, String)] -> Metadata -> Bool
isPreview env metadata = isDraft metadata && isJust (lookup "SITE_PREVIEW" env)

isDraft :: Metadata -> Bool
isDraft = not . isPublished
