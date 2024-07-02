module Green.Site.Sitemap where

import Green.Common
import Green.Site.Blog (buildBlogTags, loadPublishedPosts)
import Hakyll qualified as H
import Green.Hakyllbars as HB

sitemap :: Context String -> Rules ()
sitemap siteContext =
  match "sitemap.xml" do
    route idRoute
    compile do
      context <- sitemapContext siteContext
      getResourceBody >>= applyTemplates do
        applyContext context
        applyAsTemplate

sitemapContext :: Context String -> Compiler (Context String)
sitemapContext siteContext = do
  tagsPages <- H.loadAll . tagsPattern =<< buildBlogTags :: Compiler [Item String]
  blogPage <- H.load "blog.html" :: Compiler (Item String)
  posts <- H.recentFirst =<< loadPublishedPosts
  pages <- H.loadAll pagesPattern :: Compiler [Item String]
  feeds <- H.loadAll ("rss.xml" .||. "atom.xml") :: Compiler [Item String]
  let allPages =
        pages
          <> [blogPage]
          <> feeds
          <> tagsPages
          <> posts
      context =
        constField "updated" (latestPostUpdated posts)
          <> itemsField "pages" context allPages
          <> siteContext
  return context
  where
    pagesPattern =
      foldl1 (.||.) $
        [ H.fromGlob "_pages/**",
          "index.html",
          "archives.html"
        ]
    latestPostUpdated (latestPost : _) = tplWithItem latestPost (unContext siteContext "updated")
    latestPostUpdated [] = tplTried "latest post updated"
    tagsPattern tags = H.fromList (H.tagsMap tags <&> \(tag, _) -> H.tagsMakeId tags tag)
