module Green.Content.Sitemap (sitemapRules) where

import Green.Common

sitemapRules :: Rules ()
sitemapRules =
  create ["sitemap.xml"] do
    route idRoute
    compile sitemapCompiler

sitemapCompiler :: Compiler (Item String)
sitemapCompiler = makeItem "sitemap"
