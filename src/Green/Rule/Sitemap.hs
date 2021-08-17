module Green.Rule.Sitemap (sitemapRules) where

import Green.Common
import Green.Config

sitemapRules :: SiteConfig -> Rules ()
sitemapRules config =
  create ["sitemap.xml"] do
    route idRoute
    compile $ sitemapCompiler config

sitemapCompiler :: SiteConfig -> Compiler (Item String)
sitemapCompiler _ = makeItem "sitemap"
