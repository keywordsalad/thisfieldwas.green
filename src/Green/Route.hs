module Green.Route where

import Data.List (isSuffixOf)
import Data.String.Utils (join, split)
import Hakyll

indexRoute :: Routes
indexRoute = customRoute appendIndexHtml
  where
    appendIndexHtml = join "/" . reverse . indexIt . reverse . split "/" . toFilePath
    indexIt [] = []
    indexIt a@(x : xs)
      | x == "index.html" = a
      | ".html" `isSuffixOf` x = (head (split "." x) ++ "/index.html") : xs
      | otherwise = a

subRoute :: String -> String -> Routes
subRoute findPattern replacement =
  gsubRoute findPattern (replaceAll findPattern (const replacement))
