module Green.Site.Pages where

import Green.Common
import Green.Route
import Green.Template.Custom

pages :: Context String -> Rules ()
pages context =
  match "_pages/**" do
    route $
      gsubRoute "_pages/" (const "")
        `composeRoutes` setExtension "html"
        `composeRoutes` indexRoute
    compile $
      getResourceBody
        >>= contentCompiler context
        >>= layoutCompiler context
        >>= relativizeUrls
