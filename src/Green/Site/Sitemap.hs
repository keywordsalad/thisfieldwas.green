module Green.Site.Sitemap where

import Green.Common
import Green.Config
import Green.Site.Blog (buildBlogCategories, buildBlogTags, loadPublishedPosts)
import Green.Template
import Green.Template.Custom
import qualified Hakyll as H

sitemap :: SiteConfig -> Context String -> Rules ()
sitemap config siteContext =
  match "sitemap.xml" do
    let preview = config ^. sitePreview
    route idRoute
    compile do
      context <- sitemapContext preview siteContext
      getResourceBody >>= applyTemplates do
        applyContext context
        applyAsTemplate

sitemapContext :: Bool -> Context String -> Compiler (Context String)
sitemapContext preview siteContext = do
  categoriesPages <- H.loadAll . tagsPattern =<< buildBlogCategories preview :: Compiler [Item String]
  tagsPages <- H.loadAll . tagsPattern =<< buildBlogTags preview :: Compiler [Item String]
  blogPage <- H.load "blog.html" :: Compiler (Item String)
  posts <- H.recentFirst =<< loadPublishedPosts preview
  pages <- H.loadAll pagesPattern :: Compiler [Item String]
  feeds <- H.loadAll ("rss.xml" .||. "atom.xml") :: Compiler [Item String]
  let allPages =
        pages
          <> [blogPage]
          <> feeds
          <> categoriesPages
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
