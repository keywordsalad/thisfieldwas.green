module Site.Rule.Sitemap (sitemapRules) where

import Site.Common
import Site.Rule.Blog (loadPublishedPosts)

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
  interpolateResourceBody localConfig
    >>= relativizeUrls
