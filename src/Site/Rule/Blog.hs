module Site.Rule.Blog
  ( loadDraftPosts
  , loadPublishedPosts
  , postCtx
  , blogRules
  )
  where

import Debug.Trace
import Site.Common

type PostSnapshot = String

publishedSnapshot :: PostSnapshot
publishedSnapshot = "_publishedposts"

draftSnapshot :: PostSnapshot
draftSnapshot = "_draftposts"

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = loadExistingSnapshots "blog/*" publishedSnapshot

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadAll (fromVersion $ Just draftSnapshot)

blogRules :: [(String, String)] -> Context String -> Rules ()
blogRules env baseCtx = do
  publishedPostRules env baseCtx
  draftPostRules env baseCtx

publishedPostRules :: [(String, String)] -> Context String -> Rules ()
publishedPostRules env baseCtx =
  matchMetadata "blog/*" (isPublishable env) do
    route $ baseRoute
    compile $ postCompiler env publishedSnapshot ctx
  where
    ctx = postCtx <> baseCtx

draftPostRules :: [(String, String)] -> Context String -> Rules ()
draftPostRules env baseCtx =
  matchMetadata "blog/*" isDraft do
    route baseRoute
    compile $ postCompiler env draftSnapshot ctx
  where
    ctx = postCtx <> baseCtx

postCompiler :: [(String, String)]
  -> Snapshot
  -> Context String
  -> Compiler (Item String)
postCompiler env snapshot ctx =
  getResourceBody
  >>= applyAsTemplate ctx . maybeDebugPost env
  >>= pandocCompilerForCodeInsertion
  >>= loadAndApplyTemplate "templates/post.html" ctx
  >>= saveSnapshot snapshot
  >>= saveSnapshot "content"
  >>= loadAndApplyTemplate "templates/default.html" ctx
  >>= relativizeUrls

maybeDebugPost :: [(String, String)] -> Item String -> Item String
maybeDebugPost env item =
  let sep = "=================================================\n"
      y = toFilePath (itemIdentifier item) ++ sep
      z = itemBody item ++ sep
   in if isJust (lookup "SITE_DEBUG" env)
      then trace (sep ++ y ++ z) item
      else item

baseRoute :: Routes
baseRoute = setExtension "html" `composeRoutes` dateRoute

dateRoute :: Routes
dateRoute = gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")

isPublishable :: [(String, String)] -> Metadata -> Bool
isPublishable env metadata = or $ [isPreview env, isPublished] <*> [metadata]

isPublished :: Metadata -> Bool
isPublished = maybe True (== "true") . lookupString "published"

isPreview :: [(String, String)] -> Metadata -> Bool
isPreview env metadata = isDraft metadata && isJust (lookup "SITE_PREVIEW" env)

isDraft :: Metadata -> Bool
isDraft = not . isPublished
