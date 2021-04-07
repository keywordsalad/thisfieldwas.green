module Site.Compiler where

import Control.Monad.Except (catchError, (>=>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace
import Hakyll
import Text.Pandoc.Highlighting (pygments)
import qualified Text.Pandoc.Options as Opt

applyContentTemplates ::
  -- | template context
  Context String ->
  -- | the item being compiled
  Item String ->
  -- | the newly constructed compiler
  Compiler (Item String)
applyContentTemplates =
  applyTemplatesFromMetadata "contentTemplate" []

applyPageTemplates ::
  -- | template context
  Context String ->
  -- | the item being compiled
  Item String ->
  -- | the newly constructed compiler
  Compiler (Item String)
applyPageTemplates =
  applyTemplatesFromMetadata "templates" ["default", "skeleton"]

applyTemplatesFromMetadata ::
  -- | the key to read templates from
  String ->
  -- | the default templates
  [String] ->
  -- | template context
  Context String ->
  -- | the item being compiled
  Item String ->
  -- | the newly constructed compiler
  Compiler (Item String)
applyTemplatesFromMetadata key defaultTemplates ctx item = do
  applyTemplatesFromList [] ctx item -- TODO

applyTemplatesFromList ::
  -- | the list of templates to apply
  [String] ->
  -- | template context
  Context String ->
  -- | the item being compiled
  Item String ->
  -- | the newly constructed compiler
  Compiler (Item String)
applyTemplatesFromList templates ctx =
  foldl (>=>) pure templates'
  where
    templates' = toTemplate' <$> templates
    toTemplate' t = loadAndApplyTemplate (templateId t) ctx
    templateId t = fromFilePath $ "templates/" ++ t ++ ".html"

maybeLoadSnapshot :: Identifier -> Snapshot -> Compiler (Maybe (Item String))
maybeLoadSnapshot id' snapshot =
  catchError
    (Just <$> loadSnapshot id' snapshot)
    \_ -> return Nothing

loadExistingSnapshots :: Pattern -> Snapshot -> Compiler [Item String]
loadExistingSnapshots pat snapshot = do
  matching <- getMatches pat
  results <- mapM (flip maybeLoadSnapshot $ snapshot) matching
  return [x | Just x <- results]

pandocCompilerForCodeInsertion :: Item String -> Compiler (Item String)
pandocCompilerForCodeInsertion content = do
  itemPandoc <- readPandocWith defaultHakyllReaderOptions content
  itemPandoc' <- traverse return itemPandoc
  return $ writePandocWith defaultHakyllWriterOptions itemPandoc'

interpolateResourceBody :: [(String, String)] -> Context String -> Compiler (Item String)
interpolateResourceBody env ctx =
  getResourceBody
    >>= applyAsTemplate ctx . maybeDebug env
    >>= pandocCompilerForCodeInsertion

maybeDebug :: [(String, String)] -> Item String -> Item String
maybeDebug env item =
  let sep = "=================================================\n"
      y = toFilePath (itemIdentifier item) ++ sep
      z = itemBody item ++ sep
   in if isJust (lookup "SITE_DEBUG" env)
        then trace (sep ++ y ++ z) item
        else item

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
    { Opt.writerHTMLMathMethod = Opt.MathJax "",
      Opt.writerHighlightStyle = Just pygments
    }
