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

applyTemplates :: (Compiler (Item a), Context a) -> TemplateRunner a () -> Compiler (Item a)
applyTemplates (itemM, context) =
  applyTemplatesWith itemM context

applyTemplatesWith :: Compiler (Item a) -> Context a -> TemplateRunner a () -> Compiler (Item a)
applyTemplatesWith itemM context templates =
  itemM >>= evalStateT (templates >> tplItem) . templateRunner context

contentTemplate :: TemplateRunner String ()
contentTemplate = do
  applyAsTemplate
  tplModifyItem $ lift . pandocCompiler

layoutTemplate :: TemplateRunner String ()
layoutTemplate = loadAndApplyTemplate "_layouts/from-context.html"

fileTemplate :: FilePath -> TemplateRunner String ()
fileTemplate filePath =
  loadAndApplyTemplate (fromFilePath filePath)

withCompiler :: (Item a -> Compiler (Item a)) -> TemplateRunner a ()
withCompiler compiler =
  tplModifyItem $ lift . compiler

saveSnapshots :: [String] -> TemplateRunner String ()
saveSnapshots snapshots = do
  item <- tplItem
  lift $ forM_ snapshots' (`saveSnapshot` item)
  where
    snapshots' = nub ("_content" : snapshots)
