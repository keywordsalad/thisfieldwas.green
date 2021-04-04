module Site.Compiler.CustomPandoc where

import Hakyll
import Text.Pandoc.Highlighting (pygments)
import qualified Text.Pandoc.Options as Opt

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  pandocCompilerWithTransformM readerOpts writerOpts return

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
    { Opt.writerHTMLMathMethod = Opt.MathJax ""
    , Opt.writerHighlightStyle = Just pygments
    }
