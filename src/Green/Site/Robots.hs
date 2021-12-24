module Green.Site.Robots where

import Green.Common
import Green.Template

robotsTxt :: Context String -> Rules ()
robotsTxt context = do
  match "robots.txt" do
    route idRoute
    compile $
      getResourceBody
        >>= applyAsTemplate' context
