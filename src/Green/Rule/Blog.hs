module Green.Rule.Blog where

import Green.Common
import Green.Config
import Green.Route
import Green.Template
import qualified Hakyll as H

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
{- Routes -}
{-----------------------------------------------------------------------------}

datePattern :: String
datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"

postPattern :: String
postPattern = "^_posts/([^/]+/)?" ++ datePattern

dateRoute :: Routes
dateRoute = gsubRoute datePattern (H.replaceAll "-" (const "/"))

postRoute :: Routes
postRoute =
  matchRoute (fromRegex postPattern) $
    subPrefixRoute "_posts/" "blog/"
      `composeRoutes` dateRoute
      `composeRoutes` setExtension "html"
      `composeRoutes` indexRoute

draftRoute :: Routes
draftRoute =
  subPrefixRoute "_drafts/" "blog/drafts/"
    `composeRoutes` dateRoute
    `composeRoutes` setExtension "html"
    `composeRoutes` indexRoute

{-----------------------------------------------------------------------------}
{- Compilers -}
{-----------------------------------------------------------------------------}

postCompiler :: SiteConfig -> Compiler (Item String)
postCompiler localConfig = applyAsTemplate (localConfig ^. siteContext) =<< getResourceBody

draftCompiler :: SiteConfig -> Compiler (Item String)
draftCompiler localConfig = applyAsTemplate (localConfig ^. siteContext) =<< getResourceBody

blogCompiler :: SiteConfig -> Compiler (Item String)
blogCompiler _ = makeItem "blog"

archivesCompiler :: SiteConfig -> Compiler (Item String)
archivesCompiler _ = makeItem "archives"

draftArchivesCompiler :: SiteConfig -> Compiler (Item String)
draftArchivesCompiler _ = makeItem "draft archives"
