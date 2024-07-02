module Green.Hakyllbars.Template
  ( applyTemplates,
    applyContent,
    applyContentWith,
    fileTemplate,
    applyCompiler,
    applyContext,
    applyTemplate,
    applyAsTemplate,
  )
where

import Control.Monad.State.Strict (evalStateT)
import Hakyll (defaultHakyllReaderOptions, defaultHakyllWriterOptions)
import Green.Hakyllbars.Common
import Green.Hakyllbars.Compiler (applyAsTemplate, applyTemplate)
import Green.Hakyllbars.Context
import Green.Hakyllbars.Pandoc (pandocCompilerWith)
import Text.Pandoc (ReaderOptions, WriterOptions)

applyTemplates :: TemplateRunner a () -> Item a -> Compiler (Item a)
applyTemplates templates item =
  evalStateT (templates >> tplItem) $
    TemplateState
      { tplContextStack = [],
        tplItemStack = [item],
        tplCallStack = ["item " ++ itemFilePath item]
      }

applyContent :: TemplateRunner String ()
applyContent = applyContentWith defaultHakyllReaderOptions defaultHakyllWriterOptions

applyContentWith :: ReaderOptions -> WriterOptions -> TemplateRunner String ()
applyContentWith readerOpts writerOpts = do
  applyAsTemplate
  tplModifyItem $ lift . pandocCompilerWith readerOpts writerOpts

fileTemplate :: FilePath -> TemplateRunner String ()
fileTemplate filePath =
  applyTemplate (fromFilePath filePath)

applyCompiler :: (Item a -> Compiler (Item a)) -> TemplateRunner a ()
applyCompiler compiler =
  tplModifyItem $ lift . compiler

applyContext :: Context a -> TemplateRunner a ()
applyContext = tplPushContext
{-# INLINE applyContext #-}
