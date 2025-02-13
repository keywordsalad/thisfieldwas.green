module Green.Template.Custom where

import Control.Monad (forM_)
import Data.List (nub)
import Green.Common
import Green.Hakyllbars as HB

saveSnapshots :: [String] -> TemplateRunner String ()
saveSnapshots snapshots = do
  item <- tplItem
  lift $ forM_ snapshots' (`saveSnapshot` item)
  where
    snapshots' = nub ("_content" : snapshots)

applyLayout :: TemplateRunner String ()
applyLayout = applyTemplate "_layouts/from-context.html"
