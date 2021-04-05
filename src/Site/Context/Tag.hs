module Site.Context.Tag where

import Hakyll

buildTagsCtx :: Tags -> Context String
buildTagsCtx = tagsField "tags"
