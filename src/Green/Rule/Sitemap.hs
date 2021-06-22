module Green.Rule.Sitemap (sitemapRules) where

import Green.Common
import Green.Rule.Blog (loadPublishedPosts)

sitemapRules :: SiteConfig -> Rules ()
sitemapRules config =
  create ["sitemap.xml"] do
    route idRoute
    compile $ sitemapCompiler config

sitemapCompiler :: SiteConfig -> Compiler (Item String)
sitemapCompiler config = do
  staticPages <- loadAll "*.html"
  blogIndexes <- loadAll $ fromList ["blog.html", "archives.html"]
  blogPosts <- recentFirst =<< loadPublishedPosts
  let pageField = listField "pages" (config ^. siteContext) (return $ mconcat [staticPages, blogIndexes, blogPosts])
      localConfig = config & siteContext %~ (pageField <>)
  makeItem ""
    >>= loadAndApplyTemplate
      (fromFilePath "templates/sitemap.xml")
      (localConfig ^. siteContext)
    >>= relativizeUrls
