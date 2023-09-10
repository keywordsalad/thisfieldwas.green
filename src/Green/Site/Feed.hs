module Green.Site.Feed where

import Green.Common
import Green.Site.Blog (loadPublishedPosts, postContext, teaserContext)
import Hakyll (recentFirst)
import Hakyllbars as HB

feed :: Context String -> Rules ()
feed context = do
  match "atom.xml" do
    route idRoute
    compile $ feedCompiler context
  match "rss.xml" do
    route idRoute
    compile $ feedCompiler context

feedCompiler :: Context String -> Compiler (Item String)
feedCompiler context = do
  -- get the posts
  posts <- recentFirst =<< loadPublishedPosts
  getResourceBody >>= applyTemplates do
    -- apply the root context
    applyContext context
    -- get the most recent publish date
    lastUpdated <- tplGetWithItemContext (head posts) postContext "updated"
    lastPublished <- tplGetWithItemContext (head posts) postContext "published"
    -- apply the feed context
    applyContext $
      itemsField "posts" (postContext <> teaserContext) posts
        <> constField "lastUpdated" (lastUpdated :: String)
        <> constField "lastPublished" (lastPublished :: String)
    -- render the feed
    applyAsTemplate
