module Green.Rule.Blog where

import Green.Common
import Green.Compiler
import Green.Config
import Green.Context
import Green.Route

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

loadPostsContent :: Compiler [Item String]
loadPostsContent = loadExistingSnapshots "_posts/**" (contentOnly postSnapshot)

loadDraftsContent :: Compiler [Item String]
loadDraftsContent = loadExistingSnapshots "_drafts/**" (contentOnly draftSnapshot)

{-----------------------------------------------------------------------------}
{- Routes -}
{-----------------------------------------------------------------------------}

datePattern :: String
datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"

postPattern :: String
postPattern = "^_posts/([^/]+/)?" ++ datePattern

postRoute :: Routes
postRoute =
  matchRoute (fromRegex postPattern) $
    dateRoute
      `composeRoutes` setExtension "html"
      `composeRoutes` indexRoute
      `composeRoutes` subPrefixRoute "_posts/" "blog/"

draftRoute :: Routes
draftRoute =
  setExtension "html"
    `composeRoutes` indexRoute
    `composeRoutes` subPrefixRoute "_drafts/" "blog/drafts/"

dateRoute :: Routes
dateRoute = gsubRoute datePattern (replaceAll "-" (const "/"))

{-----------------------------------------------------------------------------}
{- Compilers -}
{-----------------------------------------------------------------------------}

postCompiler :: SiteConfig -> Compiler (Item String)
postCompiler localConfig = postSnapshotCompiler localConfig postSnapshot

draftCompiler :: SiteConfig -> Compiler (Item String)
draftCompiler localConfig = postSnapshotCompiler localConfig draftSnapshot

postSnapshotCompiler :: SiteConfig -> String -> Compiler (Item String)
postSnapshotCompiler localConfig snapshot = do
  interpolateResourceBody localConfig
    >>= saveSnapshot (contentOnly snapshot)
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

blogCompiler :: SiteConfig -> Compiler (Item String)
blogCompiler config = do
  allPostsByRecent <- recentFirst =<< loadPostsContent
  let latestPost = head allPostsByRecent
  let recentPosts = take 5 . drop 1 $ allPostsByRecent
  localConfig <- forOf siteContext config (buildBlogContext latestPost recentPosts)
  interpolateResourceBody localConfig
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

archivesCompiler :: SiteConfig -> Compiler (Item String)
archivesCompiler config = do
  posts <- recentFirst =<< loadPostsContent
  let localConfig = config & siteContext %~ archivesContext posts
  interpolateResourceBody localConfig
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

draftArchivesCompiler :: SiteConfig -> Compiler (Item String)
draftArchivesCompiler config = do
  drafts <- recentFirst =<< loadDraftsContent
  let localConfig = config & siteContext %~ draftArchivesContext drafts
  interpolateResourceBody localConfig
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls

{-----------------------------------------------------------------------------}
{- Contexts -}
{-----------------------------------------------------------------------------}

buildBlogContext :: Item String -> [Item String] -> Context String -> Compiler (Context String)
buildBlogContext latestPost recentPosts siteContext' = do
  latestPostTitle <- unContextString siteContext' "title" [] latestPost
  return $
    mconcat
      [ constField "title" ("Most Recently Mowed: " ++ latestPostTitle),
        constField "latestPost" (itemBody latestPost),
        recentPostsField,
        siteContext'
      ]
  where
    recentPostsField =
      listField
        "recentPosts"
        (teaserField' <> siteContext')
        (return recentPosts)
    teaserField' = teaserField "teaser" (contentOnly postSnapshot)

archivesContext :: [Item String] -> Context String -> Context String
archivesContext posts siteContext' =
  constField "title" "Archives"
    <> listField "posts" siteContext' (return posts)
    <> siteContext'

draftArchivesContext :: [Item String] -> Context String -> Context String
draftArchivesContext drafts siteContext' =
  constField "title" "Draft Archive"
    <> listField "posts" siteContext' (return drafts)
    <> siteContext'
