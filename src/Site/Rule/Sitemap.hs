module Site.Rule.Sitemap (sitemapRules) where

import Site.Common
import Site.Context.Post
import Site.Rule.Blog (loadPublishedPosts)

sitemapRules :: Context String -> Rules ()
sitemapRules baseCtx =
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ sitemapCompiler baseCtx

sitemapCtx :: Context String -> [Item String] -> Context String
sitemapCtx baseCtx pages = do
  listField "pages" baseCtx (return pages)

sitemapCompiler :: Context String -> Compiler (Item String)
sitemapCompiler baseCtx = do
  posts <- recentFirst =<< loadPublishedPosts
  pages <- loadAll $ fromList
    [ "about-me.md"
    , "contact.md"
    ]
  let ctx = sitemapCtx (postCtx <> baseCtx) (posts <> pages) <> baseCtx
  makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" ctx
