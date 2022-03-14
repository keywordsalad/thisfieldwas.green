module Green.Template.Custom
  ( module Green.Template,
    module Green.Template.Custom,
    module Green.Template.Custom.DateField,
    module Green.Template.Custom.GitField,
    module Green.Template.Custom.HtmlField,
  )
where

import Control.Monad.State.Strict (evalStateT, forM_)
import Data.List (nub)
import Green.Common
import Green.Template
import Green.Template.Custom.DateField
import Green.Template.Custom.GitField
import Green.Template.Custom.HtmlField

applyTemplates :: TemplateRunner a () -> Item a -> Compiler (Item a)
applyTemplates templates item =
  evalStateT (templates >> tplItem) $
    TemplateState
      { tplContextStack = [],
        tplItemStack = [item],
        tplCallStack = ["item " ++ itemFilePath item]
      }

applyContent :: TemplateRunner String ()
applyContent = do
  applyAsTemplate
  tplModifyItem $ lift . pandocCompiler

applyLayout :: TemplateRunner String ()
applyLayout = applyTemplate "_layouts/from-context.html"

fileTemplate :: FilePath -> TemplateRunner String ()
fileTemplate filePath =
  applyTemplate (fromFilePath filePath)

applyCompiler :: (Item a -> Compiler (Item a)) -> TemplateRunner a ()
applyCompiler compiler =
  tplModifyItem $ lift . compiler

saveSnapshots :: [String] -> TemplateRunner String ()
saveSnapshots snapshots = do
  item <- tplItem
  lift $ forM_ snapshots' (`saveSnapshot` item)
  where
    snapshots' = nub ("_content" : snapshots)

applyContext :: Context a -> TemplateRunner a ()
applyContext = tplPushContext
{-# INLINE applyContext #-}
