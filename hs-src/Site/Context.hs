module Site.Context where

import Hakyll

buildTagsCtx :: Tags -> Context String
buildTagsCtx = tagsField "tags"
