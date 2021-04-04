module Site.Context.Post where

import Hakyll

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
