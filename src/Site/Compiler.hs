module Site.Compiler where

import Control.Monad.Except (catchError, (>=>))
import qualified Data.Aeson.Types as AT
import Data.List (foldl')
import Data.Maybe (isJust)
import Debug.Trace
import Hakyll
  ( Compiler,
    Context,
    Identifier,
    Item (itemBody, itemIdentifier),
    MonadMetadata (getMatches, getMetadata),
    Pattern,
    Snapshot,
    applyAsTemplate,
    defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    fromFilePath,
    getResourceBody,
    loadAndApplyTemplate,
    loadSnapshot,
    pandocCompilerWithTransformM,
    readPandocWith,
    toFilePath,
    writePandocWith,
  )
import Site.Metadata
import Text.Pandoc.Highlighting (pygments)
import qualified Text.Pandoc.Options as Opt

applyContentTemplates ::
  -- | the metadata config
  PageMetadataConfig ->
  -- | template context
  Context String ->
  -- | the item being compiled
  Item String ->
  -- | the newly constructed compiler
  Compiler (Item String)
applyContentTemplates config =
  applyTemplatesFromMetadata config contentTemplates

applyPageTemplates ::
  -- | the metadata config
  PageMetadataConfig ->
  -- | template context
  Context String ->
  -- | the item being compiled
  Item String ->
  -- | the newly constructed compiler
  Compiler (Item String)
applyPageTemplates config =
  applyTemplatesFromMetadata config templates

applyTemplatesFromMetadata ::
  -- | the metadata config
  PageMetadataConfig ->
  -- | the key to read templates from
  (PageMetadata -> [String]) ->
  -- | template context
  Context String ->
  -- | the item being compiled
  Item String ->
  -- | the newly constructed compiler
  Compiler (Item String)
applyTemplatesFromMetadata config f ctx item = do
  let id' = itemIdentifier item
  metadata <- getMetadata id'
  pageMetadata <- case AT.parse (parsePageMetadata config) metadata of
    AT.Error s -> fail s
    AT.Success pm -> return pm
  applyTemplatesFromList (f pageMetadata) ctx item

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
  foldl' (>=>) pure templates'
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
