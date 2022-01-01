module Green.Template.Pandoc where

import Hakyll
import System.FilePath
import Text.Pandoc

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
readerOpts =
  defaultHakyllReaderOptions
    { readerExtensions =
        foldl (flip ($)) (readerExtensions defaultHakyllReaderOptions) $
          [ enableExtension Ext_smart,
            disableExtension Ext_markdown_in_html_blocks
          ]
    }

writerOpts :: WriterOptions
writerOpts = defaultHakyllWriterOptions
