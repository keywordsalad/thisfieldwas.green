module Green.Site.Pages where

import Control.Monad (forM_)
import Green.Common
import Green.Route
import Green.Template.Custom
import Hakyll (fromGlob)

pages :: Context String -> Rules ()
pages context = forM_ ["_pages/", "_errors/"] \dir -> do
  match (fromGlob $ dir ++ "**") do
    route $
      gsubRoute dir (const "")
        `composeRoutes` setExtension "html"
        `composeRoutes` indexRoute
    compile $
      (getResourceBody, context) `applyTemplates` do
        contentTemplate
        layoutTemplate
