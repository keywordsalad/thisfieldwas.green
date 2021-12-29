module Green.Site.Robots where

import Green.Common
import Green.Template
import Green.Template.Custom

robotsTxt :: Context String -> Rules ()
robotsTxt context = do
  match "robots.txt" do
    route idRoute
    compile $
      getResourceBody >>= applyTemplates do
        applyContext context
        applyAsTemplate
