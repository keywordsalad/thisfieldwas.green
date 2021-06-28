module Green.Rule.Feed (feedRules) where

import Green.Common
import Green.Rule.Blog (loadPosts)

feedRules :: SiteConfig -> Rules ()
feedRules config = do
  let feedConfig = config ^. siteFeedConfiguration
  create ["atom.xml"] do
    route idRoute
    compile $ feedCompiler config (renderAtom feedConfig)
  create ["rss.xml"] do
    route idRoute
    compile $ feedCompiler config (renderRss feedConfig)

feedCompiler :: SiteConfig -> RenderFeed -> Compiler (Item String)
feedCompiler config renderFeed = do
  posts <- fmap (take 10) . recentFirst =<< loadPosts
  renderFeed (feedContext <> config ^. siteContext) posts

feedContext :: Context String
feedContext = bodyField "description" <> postContext
