module Site.Compiler.CustomPandoc where

import Hakyll
import qualified Text.Pandoc.Options as Opt

customPandocExtensions :: Opt.Extensions
customPandocExtensions =
  Opt.pandocExtensions <> customExtensions
  where
    customExtensions =
      Opt.extensionsFromList
        [ Opt.Ext_emoji
        ]

customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  pandocCompilerWithTransformM readerOpts writerOpts return

readerOpts :: Opt.ReaderOptions
readerOpts =
  defaultHakyllReaderOptions
    { Opt.readerExtensions = defaultExtensions <> customPandocExtensions
    }
  where
    defaultExtensions = Opt.readerExtensions defaultHakyllReaderOptions

writerOpts :: Opt.WriterOptions
writerOpts =
  defaultHakyllWriterOptions
    { Opt.writerHTMLMathMethod = Opt.MathJax "",
      Opt.writerExtensions = defaultExtensions <> customPandocExtensions
    }
  where
    defaultExtensions = Opt.writerExtensions defaultHakyllWriterOptions
