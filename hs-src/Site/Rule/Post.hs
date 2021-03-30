module Site.Rule.Post
  ( loadDraftPosts
  , loadPublishedPosts
  , postCtx
  , postRules
  )
  where

import Hakyll
import Site.Compiler

type PostSnapshot = String

publishedSnapshot :: PostSnapshot
publishedSnapshot = "_publishedPosts"

draftSnapshot :: PostSnapshot
draftSnapshot = "_draftPosts"

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = loadExistingSnapshots "posts/*" publishedSnapshot
--loadPublishedPosts = loadAll "posts/*"

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadAll (fromVersion $ Just draftSnapshot)

postRules :: [(String, String)] -> Context String -> Rules ()
postRules env baseCtx = do
  publishedPostRules env baseCtx
  draftPostRules baseCtx

publishedPostRules :: [(String, String)] -> Context String -> Rules ()
publishedPostRules env baseCtx =
  matchMetadata "posts/*" (isPublishable env) do
    route $ baseRoute
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" ctx
      >>= saveSnapshot publishedSnapshot
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
  where
    ctx = postCtx <> baseCtx

draftPostRules :: Context String -> Rules()
draftPostRules baseCtx =
  matchMetadata "posts/*" isDraft do
    route $ baseRoute `composeRoutes` gsubRoute "posts/" (const "drafts/")
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" ctx
      >>= saveSnapshot draftSnapshot
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
  where
    ctx = postCtx <> baseCtx

baseRoute :: Routes
baseRoute = setExtension "html" `composeRoutes` dateRoute

dateRoute :: Routes
dateRoute = gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")

isPublishable :: [(String, String)] -> Metadata -> Bool
isPublishable env metadata = or $ [isPreview env, isPublished] <*> [metadata]

isPublished :: Metadata -> Bool
isPublished = maybe True (== "true") . lookupString "published"

isPreview :: [(String, String)] -> Metadata -> Bool
isPreview env metadata = isDraft metadata && lookup "HAKYLL_PREVIEW" env == Just "1"

isDraft :: Metadata -> Bool
isDraft = not . isPublished

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
