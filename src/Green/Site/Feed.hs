module Green.Site.Feed where

import Data.List (sortOn)
import Data.Time
import Green.Common
import Green.Config
import Green.Site.Blog (loadPublishedPosts, postContext, teaserContext)
import Green.Template
import Green.Template.Custom
import Green.Util (maybeHead)
import Hakyll (recentFirst)

feed :: SiteConfig -> Context String -> Rules ()
feed config context = do
  match "atom.xml" do
    route idRoute
    compile $ feedCompiler config context
  match "rss.xml" do
    route idRoute
    compile $ feedCompiler config context

feedCompiler :: SiteConfig -> Context String -> Compiler (Item String)
feedCompiler config context = do
  -- get the posts
  posts <- recentFirst =<< loadPublishedPosts (config ^. sitePreview)
  -- estimate the most recent change
  lastUpdatedUTCs <- mapM (getLastModifiedDate $ config ^. siteTimeLocale) posts
  let lastUpdatedZT = maybeHead $ sortOn zonedTimeToUTC lastUpdatedUTCs
      lastUpdated = normalizedTime (config ^. siteTimeLocale) <$> lastUpdatedZT

  getResourceBody >>= applyTemplates do
    -- apply the root context
    applyContext context
    -- get the most recent publish date
    lastPublished <- tplGetWithItemContext (head posts) postContext "published"
    -- apply the feed context
    applyContext $
      itemsField "posts" (postContext <> teaserContext) posts
        <> constField "lastUpdated" lastUpdated
        <> constField "lastPublished" (lastPublished :: String)
    -- render the feed
    applyAsTemplate
