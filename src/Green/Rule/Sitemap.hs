module Green.Rule.Sitemap (sitemapRules) where

import Green.Common
import Green.Rule.Blog (loadPosts)

sitemapRules :: SiteConfig -> Rules ()
sitemapRules config =
  create ["sitemap.xml"] do
    route idRoute
    compile $ sitemapCompiler config

sitemapCompiler :: SiteConfig -> Compiler (Item String)
sitemapCompiler config = do
  staticPages <- loadAll "*.html"
  blogIndexes <- loadAll $ fromList ["blog/index.html", "blog/archives.html"]
  posts <- recentFirst =<< loadPosts
  let pageField = listField "pages" (config ^. siteContext) (return $ mconcat [staticPages, blogIndexes, posts])
      localConfig = config & siteContext %~ (pageField <>)
  makeItem ""
    >>= loadAndApplyTemplate
      (fromFilePath "_templates/sitemap.xml")
      (localConfig ^. siteContext)
    >>= relativizeUrls
