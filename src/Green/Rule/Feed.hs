module Green.Rule.Feed (feedRules) where

import Green.Common
import Green.Config

feedRules :: SiteConfig -> Rules ()
feedRules config = do
  create ["atom.xml"] do
    route idRoute
    compile $ feedCompiler config
  create ["rss.xml"] do
    route idRoute
    compile $ feedCompiler config

feedCompiler :: SiteConfig -> Compiler (Item String)
feedCompiler _ = makeItem "feed"
