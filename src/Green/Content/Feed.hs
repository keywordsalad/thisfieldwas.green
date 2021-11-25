module Green.Content.Feed (feed) where

import Green.Common

feed :: Rules ()
feed = do
  create ["atom.xml"] do
    route idRoute
    compile feedCompiler
  create ["rss.xml"] do
    route idRoute
    compile feedCompiler

feedCompiler :: Compiler (Item String)
feedCompiler = makeItem "feed"
