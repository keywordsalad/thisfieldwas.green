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
    { Opt.readerExtensions = defaultExtensions <> extensions
    }
  where
    defaultExtensions = Opt.readerExtensions defaultHakyllReaderOptions
    extensions = Opt.extensionsFromList
      [ Opt.Ext_tex_math_dollars
      , Opt.Ext_tex_math_double_backslash
      , Opt.Ext_latex_macros
      , Opt.Ext_inline_code_attributes
      ]

writerOpts :: Opt.WriterOptions
writerOpts =
  defaultHakyllWriterOptions
    { Opt.writerHTMLMathMethod = Opt.MathJax ""
    , Opt.writerHighlightStyle = Just pygments
    }
