module Site.Rule.Page (pageRules) where

import Hakyll

pageRules :: Context String -> Rules ()
pageRules baseCtx =
  match (fromList ["about-me.md", "contact.md", "404.md"]) do
    route $ setExtension "html"
    compile $ pageCompiler baseCtx

pageCompiler :: Context String -> Compiler (Item String)
pageCompiler baseCtx = pandocCompiler
  >>= loadAndApplyTemplate "templates/default.html" baseCtx
  >>= relativizeUrls
