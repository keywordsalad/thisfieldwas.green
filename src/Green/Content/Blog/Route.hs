module Green.Content.Blog.Route where

import Green.Common
import Green.Route
import qualified Hakyll as H

datePattern :: String
datePattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}-"

postPattern :: String
postPattern = "^_posts/([^/]+/)?" ++ datePattern

dateRoute :: Routes
dateRoute = gsubRoute datePattern (H.replaceAll "-" (const "/"))

postRoute :: Routes
postRoute =
  matchRoute (fromRegex postPattern) $
    subPrefixRoute "_posts/" "blog/"
      `composeRoutes` dateRoute
      `composeRoutes` setExtension "html"
      `composeRoutes` indexRoute

draftRoute :: Routes
draftRoute =
  subPrefixRoute "_drafts/" "blog/drafts/"
    `composeRoutes` dateRoute
    `composeRoutes` setExtension "html"
    `composeRoutes` indexRoute
