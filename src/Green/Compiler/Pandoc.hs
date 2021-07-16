module Green.Compiler.Pandoc
  ( compilePandoc,
    compilePandocWith,
    interpolateItem,
    interpolateResourceBody,
  )
where

import Debug.Trace
import Green.Common
import Green.Config
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (pygments)
import qualified Text.Pandoc.Options as Opt

compilePandoc :: Item String -> Compiler (Item String)
compilePandoc = compilePandocWith return

compilePandocWith :: (Item Pandoc -> Compiler (Item Pandoc)) -> Item String -> Compiler (Item String)
compilePandocWith f =
  readPandocWith readerOpts
    >=> f
    >=> traverse return
    >=> return . writePandocWith writerOpts

interpolateResourceBody :: SiteConfig -> Compiler (Item String)
interpolateResourceBody config =
  interpolateItem config =<< getResourceBody

interpolateItem :: SiteConfig -> Item String -> Compiler (Item String)
interpolateItem config =
  applyAsTemplate (config ^. siteContext) . printDebugItem config
    >=> compilePandoc

readerOpts :: Opt.ReaderOptions
readerOpts =
  defaultHakyllReaderOptions
    { Opt.readerExtensions = defaultExtensions <> customExtensions
    }
  where
    defaultExtensions = Opt.readerExtensions defaultHakyllReaderOptions
    customExtensions = Opt.extensionsFromList []

writerOpts :: Opt.WriterOptions
writerOpts =
  defaultHakyllWriterOptions
    { Opt.writerHTMLMathMethod = Opt.MathJax "",
      Opt.writerHighlightStyle = Just pygments
    }

printDebugItem :: SiteConfig -> Item String -> Item String
printDebugItem config item =
  if debugItem
    then trace (sep ++ y ++ z) item
    else item
  where
    debugItem = config ^. siteDebug . debugPrintItem == Just (itemIdentifier item)
    sep = "=================================================\n"
    y = toFilePath (itemIdentifier item) ++ sep
    z = itemBody item ++ sep
