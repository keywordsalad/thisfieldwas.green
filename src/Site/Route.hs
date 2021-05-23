module Site.Route where

import Data.String.Utils (endswith, join, split)
import Hakyll

appendIndexHtml :: Identifier -> FilePath
appendIndexHtml = join "/" . reverse . indexIt . reverse . split "/" . toFilePath
  where
    indexIt [] = []
    indexIt a@(x : xs)
      | x == "index.html" = a
      | endswith ".html" x = (head (split "." x) ++ "/index.html") : xs
      | otherwise = a

indexRoute :: Routes
indexRoute = customRoute appendIndexHtml

pageRoute :: Routes
pageRoute = stripPrefixRoute "^pages/"

metaRoute :: Routes
metaRoute = stripPrefixRoute "^meta/"

htmlPageRoute :: Routes
htmlPageRoute = pageRoute `composeRoutes` setExtension "html" `composeRoutes` indexRoute

stripPrefixRoute :: String -> Routes
stripPrefixRoute prefix = subPrefixRoute prefix ""

subPrefixRoute :: String -> String -> Routes
subPrefixRoute prefix replacement =
  gsubRoute prefix (replaceAll prefix (const replacement))
