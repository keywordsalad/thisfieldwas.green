module Site.Compiler.CustomPandoc where

import Hakyll
import qualified Text.Pandoc.Options as Opt

customPandocExtensions :: Opt.Extensions
customPandocExtensions =
  foldl (\exts f -> f exts) customExtensions disableExtensions
  where
    customExtensions =
      Opt.pandocExtensions <> Opt.extensionsFromList
        [ Opt.Ext_emoji,
          Opt.Ext_raw_html,
          Opt.Ext_attributes
        ]
    disableExtensions =
      Opt.disableExtension <$>
        [ Opt.Ext_implicit_figures
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
