module Site.Compiler where

import Hakyll
import Control.Monad.Except (catchError)

maybeLoadSnapshot :: Identifier -> Snapshot -> Compiler (Maybe (Item String))
maybeLoadSnapshot id' snapshot =
  catchError
    (Just <$> loadSnapshot id' snapshot)
    \_ -> return Nothing

loadExistingSnapshots :: Pattern -> Snapshot -> Compiler [Item String]
loadExistingSnapshots pat snapshot = do
  matching <- getMatches pat
  results <- mapM (flip maybeLoadSnapshot $ snapshot) matching
  return [x | Just x <- results]

pandocCompilerForCodeInsertion :: Item String -> Compiler (Item String)
pandocCompilerForCodeInsertion content = do
  itemPandoc <- readPandocWith defaultHakyllReaderOptions content
  itemPandoc' <- traverse (return . id) itemPandoc
  return $ writePandocWith defaultHakyllWriterOptions itemPandoc'
