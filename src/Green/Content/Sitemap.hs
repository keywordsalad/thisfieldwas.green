module Green.Content.Sitemap (sitemap) where

import Green.Common
import Green.Compiler (loadExistingSnapshots)
import Green.Content.Blog (loadPublishedPosts)
import Green.Template
import Hakyll (recentFirst)

sitemap :: Context String -> Rules ()
sitemap siteContext =
  match "sitemap.xml" do
    route idRoute
    compile do
      context <- sitemapContext siteContext
      getResourceBody
        >>= applyAsTemplate context

sitemapContext :: Context String -> Compiler (Context String)
sitemapContext siteContext = do
  pages <- concat <$> mapM (`loadExistingSnapshots` "_content") pagePatterns
  posts <- recentFirst =<< loadPublishedPosts
  let context =
        forItemField "updated" (fromFilePath <$> ["blog/index.html", "blog/archives.html"]) (\_ -> latestPostUpdated posts)
          <> constField "pages" (itemListValue context (pages <> posts))
          <> siteContext
  return context
  where
    pagePatterns =
      [ "index.html",
        "*.md",
        "blog/index.html",
        "blog/archives.html"
      ]
    latestPostUpdated (latestPost : _) = tplWithItem latestPost (unContext siteContext "updated")
    latestPostUpdated _ = tplTried "latest post updated"
