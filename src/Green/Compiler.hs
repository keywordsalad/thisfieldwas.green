module Green.Compiler where

import Control.Monad.Except (catchError)
import Hakyll

-- | Load an item snapshot if it exists.
maybeLoadSnapshot :: Identifier -> Snapshot -> Compiler (Maybe (Item String))
maybeLoadSnapshot id' snapshot =
  catchError
    (Just <$> loadSnapshot id' snapshot)
    \_ -> return Nothing

-- | Loads all item snapshots that exist for items matching a given pattern
-- and snapshot name.
loadExistingSnapshots :: Pattern -> Snapshot -> Compiler [Item String]
loadExistingSnapshots pat snapshot = do
  matching <- getMatches pat
  results <- mapM (`maybeLoadSnapshot` snapshot) matching
  return [x | Just x <- results]
