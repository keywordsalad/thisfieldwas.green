module Green.Rule.Page (pageRules) where

import Green.Common

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
pageCompiler localConfig =
  interpolateResourceBody localConfig
    >>= applyLayoutFromMetadata localConfig
    >>= relativizeUrls
