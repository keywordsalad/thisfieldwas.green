module Site.Rule.Page (pageRules) where

import Site.Common
import Site.Route (indexRoute)

pageRules :: Context String -> Rules ()
pageRules baseCtx =
  match (fromList pageList) do
    route $ setExtension "html" `composeRoutes` indexRoute
    compile $ pageCompiler baseCtx

pageCompiler :: Context String -> Compiler (Item String)
pageCompiler baseCtx =
  getResourceBody
    >>= applyAsTemplate baseCtx
    >>= pandocCompilerForCodeInsertion
    >>= loadAndApplyTemplate "templates/default.html" baseCtx
    >>= relativizeUrls

pageList :: [Identifier]
pageList =
  [ "about-me.md",
    "contact.md",
    "404.md",
    "resume.md"
  ]
