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

pandocCompilerForCodeInsertion :: Item String -> Compiler (Item String)
pandocCompilerForCodeInsertion = compilePandocWith return

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
  applyAsTemplate (config ^. siteContext) . printDebug config
    >=> pandocCompilerForCodeInsertion

printDebug :: SiteConfig -> Item String -> Item String
printDebug config item =
  let sep = "=================================================\n"
      y = toFilePath (itemIdentifier item) ++ sep
      z = itemBody item ++ sep
   in if config ^. siteDebug
        then trace (sep ++ y ++ z) item
        else item

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
