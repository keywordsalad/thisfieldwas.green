module Site.Context.Post where

import Hakyll
import Site.Context.Field

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  <> includeCodeField
