module Site.Rule.BrokenLinks (brokenLinks) where

import Data.Bifunctor (bimap)
import Data.Maybe (fromJust)
import Hakyll

brokenLinks :: Rules ()
brokenLinks = foldl (>>) (return ()) $ fmap fixBrokenLink brokenLinkTargetIds

fixBrokenLink :: (Identifier, Identifier) -> Rules ()
fixBrokenLink (brokenLink, targetId) = do
  create [brokenLink] do
    route idRoute
    compile $ compileRedirect targetId

compileRedirect :: Identifier -> Compiler (Item Redirect)
compileRedirect targetId = do
  r <- fromJust <$> getRoute targetId
  makeItem $ Redirect (toUrl r)

brokenLinkTargetIds :: [(Identifier, Identifier)]
brokenLinkTargetIds =
  bimap fromFilePath fromFilePath
    <$> [ ("about-me/index.html", "index.html")
        ]
