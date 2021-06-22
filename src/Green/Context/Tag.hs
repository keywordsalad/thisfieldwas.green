module Green.Context.Tag where

import Hakyll

buildTagsContext :: Tags -> Context String
buildTagsContext = tagsField "tags"
