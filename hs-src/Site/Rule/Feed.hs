module Site.Rule.Feed (feedRules) where

import Hakyll
import Site.Rule.Post (loadPublishedPosts, postCtx)
import Site.Util

feedRules :: FeedConfiguration -> Context String -> Rules ()
feedRules feedConfig baseCtx = do
  create ["atom.xml"] do
    route idRoute
    compile $ feedCompiler (renderAtom feedConfig) baseCtx
  create ["rss.xml"] do
    route idRoute
    compile $ feedCompiler (renderRss feedConfig) baseCtx

feedCompiler :: RenderFeed -> Context String -> Compiler (Item String)
feedCompiler renderFeed baseCtx = do
  posts <- fmap (take 10) . recentFirst =<< loadPublishedPosts
  renderFeed (feedCtx <> baseCtx) posts

feedCtx :: Context String
feedCtx = bodyField "description" <> postCtx
