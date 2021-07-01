module Site.Rule.Page (pageRules) where

import Site.Common
import Site.Route (indexRoute)

pageRules :: Context String -> Rules ()
pageRules baseCtx = do
  match (fromList pageList) do
    route $ setExtension "html" `composeRoutes` indexRoute
    compile $ pageCompiler baseCtx

pageCompiler :: Context String -> Compiler (Item String)
pageCompiler baseCtx =
  getResourceBody
    >>= applyAsTemplate baseCtx
    >>= pandocCompilerForCodeInsertion
    >>= loadAndApplyTemplate "templates/page.html" baseCtx
    >>= loadAndApplyTemplate "templates/default.html" baseCtx
    >>= relativizeUrls

pageList :: [Identifier]
pageList =
  [ "contact.md",
    "404.md",
    "resume.md"
  ]
