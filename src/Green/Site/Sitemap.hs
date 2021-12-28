module Green.Site.Sitemap where

import Green.Common
import Green.Site.Blog (buildBlogCategories, buildBlogTags, loadPublishedPosts)
import Green.Template
import Green.Template.Custom
import Hakyll ((.||.))
import qualified Hakyll as H

sitemap :: Context String -> Rules ()
sitemap siteContext =
  match "sitemap.xml" do
    route idRoute
    compile do
      context <- sitemapContext siteContext
      (getResourceBody, context) `applyTemplates` applyAsTemplate

sitemapContext :: Context String -> Compiler (Context String)
sitemapContext siteContext = do
  categoriesPages <- H.loadAll . tagsPattern =<< buildBlogCategories :: Compiler [Item String]
  tagsPages <- H.loadAll . tagsPattern =<< buildBlogTags :: Compiler [Item String]
  blogPage <- H.load "blog.html" :: Compiler (Item String)
  posts <- H.recentFirst =<< loadPublishedPosts
  pages <- H.loadAll pagesPattern :: Compiler [Item String]
  let allPages =
        pages
          <> [blogPage]
          <> categoriesPages
          <> tagsPages
          <> posts
      context =
        constField "updated" (latestPostUpdated posts)
          <> constField "pages" (itemListValue context allPages)
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
