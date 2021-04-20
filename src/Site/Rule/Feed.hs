module Site.Rule.Feed (feedRules) where

import Site.Common
import Site.Context.Post
import Site.Rule.Blog (loadPublishedPosts)
import Site.Util

feedRules :: Context String -> Rules ()
feedRules baseCtx = do
  create ["atom.xml"] do
    route idRoute
    compile $ feedCompiler (renderAtom feedConfig) baseCtx
  create ["rss.xml"] do
    route idRoute
    compile $ feedCompiler (renderRss feedConfig) baseCtx
  where
    feedConfig =

feedCompiler :: RenderFeed -> Context String -> Compiler (Item String)
feedCompiler renderFeed baseCtx = do
  posts <- fmap (take 10) . recentFirst =<< loadPublishedPosts
  renderFeed (feedCtx <> baseCtx) posts

feedCtx :: Context String
feedCtx = bodyField "description" <> postCtx
