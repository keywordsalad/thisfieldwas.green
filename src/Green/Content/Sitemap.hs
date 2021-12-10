module Green.Content.Sitemap (sitemap) where

import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Config
import Green.Content.Blog (loadPublishedPosts)
import Green.Template
import Hakyll (recentFirst)

sitemap :: SiteConfig -> Context String -> Rules ()
sitemap config siteContext =
  match "sitemap.xml" do
    route idRoute
    compile do
      context <- sitemapContext config siteContext
      getResourceBody
        >>= applyAsTemplate context

sitemapContext :: SiteConfig -> Context String -> Compiler (Context String)
sitemapContext config siteContext = do
  pages <- concat <$> mapM (`loadExistingSnapshots` "_content") pagePatterns
  posts <- recentFirst =<< loadPublishedPosts config
  let context =
        forItemField "updated" latestPostPatterns (\_ -> latestPostUpdated posts)
          <> constField "pages" (itemListValue context (pages <> posts))
          <> siteContext
  return context
  where
    pagePatterns =
      [ "index.html",
        "*.md",
        "blog.html",
        "archives.html"
      ]
    latestPostPatterns =
      fromFilePath
        <$> [ "blog.html",
              "archives.html",
              "categories.html",
              "tags.html"
            ]
    latestPostUpdated (latestPost : _) = tplWithItem latestPost (unContext siteContext "updated")
    latestPostUpdated _ = tplTried "latest post updated"
