module Green.Site.BrokenLinks where

import Green.Common
import Hakyll (preprocess)

brokenLinks :: Rules ()
brokenLinks = do
  pairs <- preprocess $ mapM splitLine . lines =<< readFile "broken-links.cfg"
  foldl (>>) (return ()) $ fmap createRedirect pairs
  where
    splitLine = go []
    go acc (x : xs)
      | x == '|' = return (reverse acc, xs)
      | otherwise = go (x : acc) xs
    go acc [] = fail $ "Could not split broken link config, got line " ++ show (reverse acc)

createRedirect :: (String, String) -> Rules ()
createRedirect (brokenLink, target) =
  create [fromFilePath brokenLink] do
    route idRoute
    compile do
      r <- fromMaybe target <$> getRoute (fromFilePath target)
      makeItem $ Redirect (toUrl r)
