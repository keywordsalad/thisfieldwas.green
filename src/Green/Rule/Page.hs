module Green.Rule.Page (pageRules) where

import Green.Common
import Green.Config
import Green.Route
import Green.Template.Custom

pageRules :: SiteConfig -> Rules ()
pageRules config =
  match (fromList pageList) do
    route $ setExtension "html" `composeRoutes` indexRoute
    compile $ pageCompiler config
  where
    pageList =
      [ "contact.md",
        "resume.md",
        "404.md"
      ]
