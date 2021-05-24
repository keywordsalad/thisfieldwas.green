module Site.Rule.Blog where

import Site.Common

{-----------------------------------------------------------------------------}
{- Rules -}
{-----------------------------------------------------------------------------}

blogRules :: SiteConfig -> Rules ()
blogRules config =
  let rules =
        [ blogIndexRules,
          publishedPostRules,
          draftPostRules,
          draftArchiveRules
        ]
   in sequenceA_ $ rules <*> pure config

blogIndexRules :: SiteConfig -> Rules ()
blogIndexRules config =
  match "pages/blog.md" do
    route htmlPageRoute
    compile $ blogCompiler config

draftArchiveRules :: SiteConfig -> Rules ()
draftArchiveRules config = do
  match "pages/drafts.md" do
    route htmlPageRoute
    compile $ draftPostsCompiler config

publishedPostRules :: SiteConfig -> Rules ()
publishedPostRules localConfig = do
  matchMetadata "blog/**" isPublished do
    route publishedPostRoute
    compile $ postCompiler localConfig publishedSnapshot

draftPostRules :: SiteConfig -> Rules ()
draftPostRules localConfig = do
  matchMetadata "blog/**" isDraft do
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
loadPublishedPosts = loadExistingSnapshots "blog/*" publishedSnapshot

loadDraftPosts :: Compiler [Item String]
loadDraftPosts = loadExistingSnapshots "blog/*" draftSnapshot

{-----------------------------------------------------------------------------}
{- Routes -}
{-----------------------------------------------------------------------------}

postPattern :: String
postPattern = "^blog/[0-9]{4}-[0-9]{2}-[0-9]{2}-"

matchPostRoute :: Routes -> Routes
matchPostRoute = matchRoute (fromRegex postPattern)

publishedPostRoute :: Routes
publishedPostRoute =
  matchPostRoute . composeRoutesList $
    [ dateRoute,
      setExtension "html",
      indexRoute
    ]

draftPostRoute :: Routes
draftPostRoute =
  matchPostRoute . composeRoutesList $
    [ publishedPostRoute,
      gsubRoute "^blog/" (replaceAll "^blog/" (const "drafts/"))
    ]

dateRoute :: Routes
dateRoute = gsubRoute postPattern (replaceAll "-" (const "/"))

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

  interpolateResourceBody localConfig
    >>= applyLayoutFromMetadata config
    >>= relativizeUrls

draftPostsCompiler :: SiteConfig -> Compiler (Item String)
draftPostsCompiler config = do
  posts <- recentFirst =<< loadDraftPosts
  let localConfig = config & siteContext %~ (draftArchiveContext config posts <>)
  makeItem ""
    >>= interpolateItem localConfig
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

{-----------------------------------------------------------------------------}
{- Metadata -}
{-----------------------------------------------------------------------------}

isPublished :: Metadata -> Bool
isPublished = isJust . lookupString "published"

isDraft :: Metadata -> Bool
isDraft = not . isPublished
