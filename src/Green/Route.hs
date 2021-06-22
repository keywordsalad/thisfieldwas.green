module Green.Route where

import Data.List (isSuffixOf)
import Data.String.Utils (join, split)
import Hakyll

appendIndexHtml :: Identifier -> FilePath
appendIndexHtml = join "/" . reverse . indexIt . reverse . split "/" . toFilePath
  where
    indexIt [] = []
    indexIt a@(x : xs)
      | x == "index.html" = a
      | ".html" `isSuffixOf` x = (head (split "." x) ++ "/index.html") : xs
      | otherwise = a

indexRoute :: Routes
indexRoute = customRoute appendIndexHtml

pageRoute :: Routes
pageRoute =
  composeRoutesList
    [ stripPrefixRoute "^pages/",
      setExtension "html",
      indexRoute
    ]

stripPrefixRoute :: String -> Routes
stripPrefixRoute prefix = subPrefixRoute prefix ""

subPrefixRoute :: String -> String -> Routes
subPrefixRoute prefix replacement =
  gsubRoute prefix (replaceAll prefix (const replacement))

composeRoutesList :: [Routes] -> Routes
composeRoutesList = foldr composeRoutes idRoute
