module Site.Context.Post where

import Hakyll

postContext :: Context String
postContext =
  dateField "published" "%B %e, %Y"
    <> dateField "created" "%B %e, %Y"
    <> dateField "updated" "%B %e, %Y"
