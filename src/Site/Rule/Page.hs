module Site.Rule.Page (pageRules) where

import Site.Common
import Site.Route (indexRoute)

pageRules :: [(String, String)] -> Context String -> Rules ()
pageRules env baseCtx = do
  match (fromList pageList) do
    route $ setExtension "html" `composeRoutes` indexRoute
    compile $ pageCompiler env baseCtx
  match "404.md" do
    route $ setExtension "html"
    compile $ pageCompiler env baseCtx

pageList :: [Identifier]
pageList =
  [ "about-me.md",
    "contact.md",
    "resume.md"
  ]

pageCompiler :: [(String, String)] -> Context String -> Compiler (Item String)
pageCompiler env baseCtx =
  interpolateResourceBody env baseCtx
    >>= applyPageTemplate baseCtx
    >>= relativizeUrls
