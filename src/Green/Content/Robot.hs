module Green.Content.Robot (robotsTxt) where

import Green.Common
import Green.Template

robotsTxt :: Context String -> Rules ()
robotsTxt context = do
  create ["robots.txt"] do
    route idRoute
    compile $
      makeItem ""
        >>= loadAndApplyTemplate (fromFilePath "_templates/robots.txt") context
