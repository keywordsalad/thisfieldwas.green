module Green.Rule.Blog where

import Green.Common

{-----------------------------------------------------------------------------}
{- Rules -}
{-----------------------------------------------------------------------------}

blogRules :: SiteConfig -> Rules ()
blogRules config =
  let rules =
        [ blogIndexRules,
          archiveRules,
          draftArchiveRules,
          postRules,
          draftRules
        ]
   in sequenceA_ $ rules <*> pure config

blogIndexRules :: SiteConfig -> Rules ()
blogIndexRules config =
  match "blog/index.html" do
    route idRoute
    compile $ blogCompiler config

archiveRules :: SiteConfig -> Rules ()
archiveRules config = do
  match "blog/archives.html" do
    route indexRoute
    compile $ archivesCompiler config

draftArchiveRules :: SiteConfig -> Rules ()
draftArchiveRules config = do
  match "blog/drafts.html" do
    route indexRoute
    compile $ draftArchivesCompiler config

postRules :: SiteConfig -> Rules ()
postRules localConfig = do
  match "_posts/**" do
    route postRoute
    compile $ postCompiler localConfig

draftRules :: SiteConfig -> Rules ()
draftRules localConfig = do
  match "_drafts/**" do
    route draftRoute
    compile $ draftCompiler localConfig

{-----------------------------------------------------------------------------}
{- Snapshots -}
{-----------------------------------------------------------------------------}

type PostSnapshot = String

postSnapshot :: PostSnapshot
postSnapshot = "_post"

draftSnapshot :: PostSnapshot
draftSnapshot = "_draft"

contentOnly :: PostSnapshot -> PostSnapshot
contentOnly = (++ "_contentOnly")

loadPosts :: Compiler [Item String]
loadPosts = loadExistingSnapshots "_posts/**" (contentOnly postSnapshot)

loadDrafts :: Compiler [Item String]
loadDrafts = loadExistingSnapshots "_drafts/**" (contentOnly draftSnapshot)

{-----------------------------------------------------------------------------}
{- Routes -}
{-----------------------------------------------------------------------------}

datePattern :: String
datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"

postPattern :: String
postPattern = "^_posts/([^/]+/)?" ++ datePattern

postRoute :: Routes
postRoute =
  matchRoute (fromRegex postPattern) . mconcat $
    [ dateRoute,
      setExtension "html",
      indexRoute,
      subPrefixRoute "_posts/" "blog/"
    ]

draftRoute :: Routes
draftRoute =
  mconcat
    [ setExtension "html",
      indexRoute,
      subPrefixRoute "_drafts/" "blog/drafts/"
    ]

dateRoute :: Routes
dateRoute = gsubRoute datePattern (replaceAll "-" (const "/"))

{-----------------------------------------------------------------------------}
{- Compilers -}
{-----------------------------------------------------------------------------}

postCompiler :: SiteConfig -> Compiler (Item String)
postCompiler localConfig = postSnapshotCompiler localConfig postSnapshot

draftCompiler :: SiteConfig -> Compiler (Item String)
draftCompiler localConfig = postSnapshotCompiler localConfig postSnapshot

postSnapshotCompiler :: SiteConfig -> String -> Compiler (Item String)
postSnapshotCompiler localConfig snapshot = do
  interpolateResourceBody localConfig
    >>= saveSnapshot (contentOnly snapshot)
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

blogCompiler :: SiteConfig -> Compiler (Item String)
blogCompiler config = do
  allPostsByRecent <- recentFirst =<< loadPosts
  let latestPost = head allPostsByRecent
  let recentPosts = take 5 . drop 1 $ allPostsByRecent
  let localConfig = config & siteContext %~ blogContext latestPost recentPosts
  interpolateResourceBody localConfig
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

archivesCompiler :: SiteConfig -> Compiler (Item String)
archivesCompiler config = do
  posts <- recentFirst =<< loadPosts
  let localConfig = config & siteContext %~ archivesContext posts
  interpolateResourceBody localConfig
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

draftArchivesCompiler :: SiteConfig -> Compiler (Item String)
draftArchivesCompiler config = do
  drafts <- recentFirst =<< loadDrafts
  let localConfig = config & siteContext %~ draftArchivesContext drafts
  interpolateResourceBody localConfig
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

{-----------------------------------------------------------------------------}
{- Contexts -}
{-----------------------------------------------------------------------------}

blogContext :: Item String -> [Item String] -> Context String -> Context String
blogContext latestPost recentPosts siteContext' = do
  constField "latestPost" (itemBody latestPost)
    <> recentPostsField
    <> siteContext'
  where
    teaserField' = teaserField "teaser" (contentOnly postSnapshot)
    recentPostsField = listField "recentPosts" (teaserField' <> siteContext') (return recentPosts)

archivesContext :: [Item String] -> Context String -> Context String
archivesContext posts siteContext' =
  constField "title" "Archives"
    <> listField "posts" siteContext' (return posts)
    <> siteContext'

draftArchivesContext :: [Item String] -> Context String -> Context String
draftArchivesContext drafts siteContext' = do
  constField "title" "Draft Archive"
    <> listField "posts" siteContext' (return drafts)
    <> siteContext'
