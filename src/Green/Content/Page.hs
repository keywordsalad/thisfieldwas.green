module Green.Content.Page (pages) where

import Green.Common
import Green.Route
import Green.Template.Custom

pages :: Context String -> Rules ()
pages context =
  match (fromList pageList) do
    route $ setExtension "html" `composeRoutes` indexRoute
    compile $
      getResourceBody
        >>= contentCompiler context
        >>= layoutCompiler context
        >>= relativizeUrls
  where
    pageList =
      [ "contact.md",
        "resume.md",
        "404.md"
      ]
