module Green.Content.Robot (robotsTxt) where

import Green.Common
import Green.Template

robotsTxt :: Context String -> Rules ()
robotsTxt context = do
  match "robots.txt" do
    route idRoute
    compile $
      getResourceBody
        >>= applyAsTemplate context
