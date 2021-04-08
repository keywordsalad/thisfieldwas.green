module Site.Util where

import Data.String.Utils as S
import Site.Common
import System.FilePath (splitFileName, takeDirectory)

-- | Feed renderer signature
type RenderFeed =
  Context String ->
  [Item String] ->
  Compiler (Item String)

loadAbsRoot :: IO String
loadAbsRoot = ("https://" ++) . S.strip <$> readFile "CNAME"

buildSiteRoot :: Compiler String
buildSiteRoot = toSiteRoot . fromJust <$> (getUnderlying >>= getRoute)

cleanIndexPaths :: String -> Context a
cleanIndexPaths key = mapContext transform (urlField key)
  where
    transform url = case splitFileName url of
      (p, "index.html") -> takeDirectory p
      _ -> url

stripSuffix :: String -> String -> String
stripSuffix suffix text =
  if drop prefixLength text == suffix
    then prefix
    else text
  where
    prefixLength = length text - length suffix
    prefix = take prefixLength text
