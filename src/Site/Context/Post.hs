module Site.Context.Post where

import Hakyll

postContext :: Context String
postContext =
  dateField "date" "%B %e, %Y"
