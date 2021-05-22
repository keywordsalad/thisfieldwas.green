module Site.Route where

import Data.List (isSuffixOf)
import Hakyll

indexRoute :: Routes
indexRoute = gsubRoute "\\.html$" replaceWithIndex
  where
    replaceWithIndex s
      | "/index.html" `isSuffixOf` s = s
      | otherwise = replaceAll "\\.html$" (const "/index.html") s

pageRoute :: Routes
pageRoute = stripPrefixRoute "^pages/"

metaRoute :: Routes
metaRoute = stripPrefixRoute "^meta/"

htmlPageRoute :: Routes
htmlPageRoute = mconcat [setExtension "html", pageRoute, indexRoute]

stripPrefixRoute :: String -> Routes
stripPrefixRoute prefix = subPrefixRoute prefix ""

subPrefixRoute :: String -> String -> Routes
subPrefixRoute prefix replacement =
  gsubRoute prefix (replaceAll prefix (const replacement))
