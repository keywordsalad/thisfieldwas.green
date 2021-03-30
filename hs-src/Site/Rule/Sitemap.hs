module Site.Rule.Sitemap (sitemapRules) where

import Hakyll
import Site.Rule.Post (postCtx)

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
  posts <- recentFirst =<< loadAll "posts/*"
  pages <- loadAll $ fromList
    [ "about.md"
    , "contact.md"
    ]
  let ctx = sitemapCtx (postCtx <> baseCtx) (posts <> pages) <> baseCtx
  makeItem "" >>= loadAndApplyTemplate "templates/sitemap.xml" ctx
