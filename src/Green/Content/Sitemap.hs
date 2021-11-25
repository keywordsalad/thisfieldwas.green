module Green.Content.Sitemap (sitemap) where

import Green.Common

sitemap :: Rules ()
sitemap =
  create ["sitemap.xml"] do
    route idRoute
    compile sitemapCompiler

sitemapCompiler :: Compiler (Item String)
sitemapCompiler = makeItem "sitemap"
