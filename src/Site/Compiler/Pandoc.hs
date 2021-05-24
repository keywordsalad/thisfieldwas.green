module Site.Compiler.Pandoc
  ( interpolateResourceBody,
    compilePandocWith,
    interpolateItem,
  )
where

import Control.Monad ((>=>))
import Debug.Trace
import Hakyll
import Lens.Micro
import Site.Config
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (pygments)
import qualified Text.Pandoc.Options as Opt

compilePandocWithDefault :: Item String -> Compiler (Item String)
compilePandocWithDefault = compilePandocWith return

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
    >=> compilePandocWithDefault

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
  if config ^. siteDebug
    then trace (sep ++ y ++ z) item
    else item
  where
    sep = "=================================================\n"
    y = toFilePath (itemIdentifier item) ++ sep
    z = itemBody item ++ sep
