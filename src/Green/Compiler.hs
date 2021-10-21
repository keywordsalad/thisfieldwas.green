module Green.Compiler where

import Data.Maybe (catMaybes)
import Green.Common

-- | Load an item snapshot if it exists.
maybeLoadSnapshot :: Identifier -> Snapshot -> Compiler (Maybe (Item String))
maybeLoadSnapshot id' snapshot = catchError go onError
  where
    go = Just <$> loadSnapshot id' snapshot
    onError errors = do
      debugCompiler $
        ("Errors occurred while trying to load " ++ toFilePath id')
          ++ (" for snapshot " ++ show snapshot)
          ++ (": " ++ show errors)
      return Nothing

-- | Loads all item snapshots that exist for items matching a given pattern
-- and snapshot name.
loadExistingSnapshots :: Pattern -> Snapshot -> Compiler [Item String]
loadExistingSnapshots pattern' snapshot = withErrorMessage errorMessage do
  matching <- getMatches pattern'
  debugCompiler $
    ("Found " ++ show (length matching))
      ++ (" items for snapshot " ++ show snapshot)
      ++ (" matching " ++ show pattern')
  resultMaybes <- mapM (`maybeLoadSnapshot` snapshot) matching <|> error "GOT IT"
  debugCompiler $ "Tried loading " ++ show (length resultMaybes)
  let results = catMaybes resultMaybes
  debugCompiler $ "Successfully loaded " ++ show (length results)
  return results
  where
    errorMessage = "Tried loading snapshot " ++ show snapshot ++ " with pattern " ++ show pattern'
