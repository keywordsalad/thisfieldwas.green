module Green.Compiler.Pandoc
  ( compilePandoc,
    compilePandocWith,
    interpolateItem,
    interpolateResourceBody,
  )
where

import Control.Monad ((>=>))
import Debug.Trace
import Green.Config
import Hakyll
import Lens.Micro
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
  if config ^. siteDebug . debugPrintItem
    then trace (sep ++ y ++ z) item
    else item
  where
    sep = "=================================================\n"
    y = toFilePath (itemIdentifier item) ++ sep
    z = itemBody item ++ sep
