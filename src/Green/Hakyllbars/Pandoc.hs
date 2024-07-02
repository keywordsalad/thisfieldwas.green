module Green.Hakyllbars.Pandoc where

import Hakyll hiding (pandocCompilerWith)
import System.FilePath
import Text.Pandoc

-- | Compiles an item using the pandoc renderer with the default options if it isn't already HTML.
pandocCompiler :: Item String -> Compiler (Item String)
pandocCompiler = pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions

-- | Compiles an item using the pandoc renderer with the given options if it isn't already HTML.
pandocCompilerWith :: ReaderOptions -> WriterOptions -> Item String -> Compiler (Item String)
pandocCompilerWith readerOpts writerOpts item@(Item id' _) = do
  let ext = takeExtension $ toFilePath id'
  go ext
  where
    go ".html" = return item
    go _ = renderPandocWith readerOpts writerOpts item
