module Site.Rule.Meta where

import Site.Common
import Site.Rule.Blog (loadPublishedPosts)

robotsTxtRules :: SiteConfig -> Rules ()
robotsTxtRules config = do
  match "meta/robots.txt" do
    route metaRoute
    compile $ applyAsTemplate (config ^. siteContext) =<< getResourceBody

sitemapRules :: SiteConfig -> Rules ()
sitemapRules config =
  match "meta/sitemap.xml" $ do
    route metaRoute
    compile $ sitemapCompiler config

sitemapCompiler :: SiteConfig -> Compiler (Item String)
sitemapCompiler config = do
  posts <- recentFirst =<< loadPublishedPosts
  pages <- loadAll "pages/**"
  let pageField = listField "pages" (config ^. siteContext) (return $ posts <> pages)
      localConfig = config & siteContext %~ (pageField <>)
  makeItem ""
    >>= applyAsTemplate (localConfig ^. siteContext)
