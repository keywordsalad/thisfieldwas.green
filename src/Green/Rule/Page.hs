module Green.Rule.Page (pageRules) where

import Green.Common
import Green.Compiler
import Green.Config
import Green.Route

pageRules :: SiteConfig -> Rules ()
pageRules baseCtx =
  match (fromList pageList) do
    route $ setExtension "html" `composeRoutes` indexRoute
    compile $ pageCompiler baseCtx
  where
    pageList =
      [ "contact.md",
        "resume.md",
        "404.md"
      ]

pageCompiler :: SiteConfig -> Compiler (Item String)
pageCompiler config =
  interpolateResourceBody config
    >>= applyLayout config
    >>= relativizeUrls
