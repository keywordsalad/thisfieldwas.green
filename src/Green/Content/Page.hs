module Green.Content.Page (pageRules) where

import Green.Common
import Green.Route
import Green.Template.Custom

pageRules :: Context String -> Rules ()
pageRules context =
  match (fromList pageList) do
    route $ setExtension "html" `composeRoutes` indexRoute
    compile $ pageCompiler context =<< getResourceBody
  where
    pageList =
      [ "contact.md",
        "resume.md",
        "404.md"
      ]
