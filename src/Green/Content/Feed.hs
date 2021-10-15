module Green.Content.Feed (feedRules) where

import Green.Common

feedRules :: Rules ()
feedRules = do
  create ["atom.xml"] do
    route idRoute
    compile feedCompiler
  create ["rss.xml"] do
    route idRoute
    compile feedCompiler

feedCompiler :: Compiler (Item String)
feedCompiler = makeItem "feed"
