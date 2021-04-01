module Site.Util where

import Data.Char
import Site.Common
import System.FilePath (splitFileName, takeDirectory)

type RenderFeed =
  Context String -- Item context
    -> [Item String] -- Feed items
    -> Compiler (Item String) -- Resulting feed

loadAbsRoot :: IO String
loadAbsRoot = ("https://" ++) <$> strip <$> readFile "CNAME"

buildSiteRoot :: Compiler String
buildSiteRoot = toSiteRoot . fromJust <$> (getUnderlying >>= getRoute)

cleanIndexPaths :: String -> Context a
cleanIndexPaths key = mapContext transform (urlField key)
  where
    transform url = case splitFileName url of
      (p, "index.html") -> takeDirectory p
      _                 -> url

strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace
