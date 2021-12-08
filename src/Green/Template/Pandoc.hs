module Green.Template.Pandoc where

import Hakyll
import System.FilePath
import Text.Pandoc hiding (readers, writers)

pandocCompiler :: Item String -> Compiler (Item String)
pandocCompiler item@(Item id' _) = do
  let ext = takeExtension $ toFilePath id'
  go ext
  where
    go ".html" = return item
    go _ = do
      pandoc <- readPandocWith readerOpts item
      return $ writePandocWith writerOpts pandoc

readerOpts :: ReaderOptions
readerOpts = defaultHakyllReaderOptions

writerOpts :: WriterOptions
writerOpts = defaultHakyllWriterOptions
