module Site.Rule.Blog where

import Site.Common

{-----------------------------------------------------------------------------}
{- Rules -}
{-----------------------------------------------------------------------------}

blogRules :: SiteConfig -> Rules ()
blogRules config =
  let rules =
        [ blogIndexRules,
          archiveRules,
          draftArchiveRules,
          publishedPostRules,
          draftPostRules
        ]
   in sequenceA_ $ rules <*> pure config

blogIndexRules :: SiteConfig -> Rules ()
blogIndexRules config =
  create ["blog/blog.html"] do
    route $ constRoute "blog/index.html"
    compile $ blogCompiler config

archiveRules :: SiteConfig -> Rules ()
archiveRules config = do
  create ["blog/archives.html"] do
    route indexRoute
    compile $ archiveCompiler config

draftArchiveRules :: SiteConfig -> Rules ()
draftArchiveRules config = do
  create ["blog/drafts.html"] do
    route indexRoute
    compile $ draftPostsCompiler config

publishedPostRules :: SiteConfig -> Rules ()
publishedPostRules localConfig = do
  matchMetadata "posts/**" isPublished do
    route publishedPostRoute
    compile $ postCompiler localConfig publishedSnapshot

draftPostRules :: SiteConfig -> Rules ()
draftPostRules localConfig = do
  matchMetadata "posts/**" isDraft do
    route draftPostRoute
    compile $ postCompiler localConfig draftSnapshot

{-----------------------------------------------------------------------------}
{- Snapshots -}
{-----------------------------------------------------------------------------}

type PostSnapshot = String

publishedSnapshot :: PostSnapshot
publishedSnapshot = "_publishedPost"

draftSnapshot :: PostSnapshot
draftSnapshot = "_draftPost"

loadPublishedPosts :: Compiler [Item String]
loadPublishedPosts = loadExistingSnapshots "posts/**" publishedSnapshot

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots "posts/**" draftSnapshot

{-----------------------------------------------------------------------------}
{- Routes -}
{-----------------------------------------------------------------------------}

datePattern :: String
datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"

postPattern :: String
postPattern = "^posts/([^/]+/)?" ++ datePattern

basePostRoute :: Routes
basePostRoute =
  matchRoute (fromRegex postPattern) . composeRoutesList $
    [ dateRoute,
      setExtension "html",
      indexRoute
    ]

publishedPostRoute :: Routes
publishedPostRoute = basePostRoute `composeRoutes` postDestination "blog/"

draftPostRoute :: Routes
draftPostRoute = basePostRoute `composeRoutes` postDestination "blog/drafts/"

postDestination :: String -> Routes
postDestination destinationDir =
  gsubRoute "^posts/" (replaceAll "^posts/" (const destinationDir))

dateRoute :: Routes
dateRoute = gsubRoute datePattern (replaceAll "-" (const "/"))

{-----------------------------------------------------------------------------}
{- Compilers -}
{-----------------------------------------------------------------------------}

postCompiler :: SiteConfig -> PostSnapshot -> Compiler (Item String)
postCompiler localConfig snapshot =
  interpolateResourceBody localConfig
    >>= saveSnapshot snapshot
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

blogCompiler :: SiteConfig -> Compiler (Item String)
blogCompiler config = do
  let recentPosts = recentFirst =<< loadPublishedPosts

  -- the most recent post
  latestPost <- head . take 1 <$> recentPosts
  -- other recent posts
  otherPosts <- take 5 . drop 1 <$> recentPosts

  -- set local config
  let localConfig = config & siteContext %~ blogContext latestPost otherPosts

  makeItem ""
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

draftPostsCompiler :: SiteConfig -> Compiler (Item String)
draftPostsCompiler config = do
  posts <- recentFirst =<< loadDraftPosts
  let localConfig = config & siteContext %~ (draftArchiveContext config posts <>)
  makeItem ""
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

archiveCompiler :: SiteConfig -> Compiler (Item String)
archiveCompiler config = do
  posts <- recentFirst =<< loadPublishedPosts
  let localConfig = config & siteContext %~ archiveContext posts
  makeItem ""
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

{-----------------------------------------------------------------------------}
{- Contexts -}
{-----------------------------------------------------------------------------}

draftArchiveContext :: SiteConfig -> [Item String] -> Context String
draftArchiveContext config posts = do
  constField "title" "Draft Archive"
    <> listField "posts" (config ^. siteContext) (return posts)

blogContext :: Item String -> [Item String] -> Context String -> Context String
blogContext latestPost otherPosts siteContext' = do
  constField "latest-post" (itemBody latestPost)
    <> listField
      "previous-posts"
      (teaserField "teaser" publishedSnapshot <> siteContext')
      (return otherPosts)
    <> siteContext'

archiveContext :: [Item String] -> Context String -> Context String
archiveContext posts siteContext' =
  constField "title" "Archives"
    <> listField "posts" siteContext' (return posts)
    <> siteContext'

{-----------------------------------------------------------------------------}
{- Metadata -}
{-----------------------------------------------------------------------------}

isPublished :: Metadata -> Bool
isPublished = isJust . lookupString "published"

isDraft :: Metadata -> Bool
isDraft = not . isPublished
