module Site.Rule.Archive (archiveRules) where

import Site.Common
import Site.Rule.Blog (loadPublishedPosts)

archiveRules :: SiteConfig -> Rules ()
archiveRules config = do
  create ["pages/archives.md"] do
    route htmlPageRoute
    compile $ archiveCompiler config

archiveContext :: [Item String] -> Context String -> Context String
archiveContext posts siteContext' =
  constField "title" "Archives"
    <> listField "posts" siteContext' (return posts)
    <> siteContext'

archiveCompiler :: SiteConfig -> Compiler (Item String)
archiveCompiler config = do
  posts <- recentFirst =<< loadPublishedPosts
  let localConfig = config & siteContext %~ archiveContext posts
  makeItem ""
    >>= interpolateItem localConfig
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls
