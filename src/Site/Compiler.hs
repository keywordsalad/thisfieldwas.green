module Site.Compiler where

import Control.Monad.Except (catchError, (>=>))
import Data.Maybe (isJust)
import Debug.Trace
import Hakyll
  ( Compiler,
    Context,
    Identifier,
    Item (itemBody, itemIdentifier),
    MonadMetadata (getMatches),
    Pattern,
    Snapshot,
    applyAsTemplate,
    defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    getResourceBody,
    loadAndApplyTemplate,
    loadSnapshot,
    pandocCompilerWithTransformM,
    readPandocWith,
    toFilePath,
    writePandocWith,
  )
import Text.Pandoc.Highlighting (pygments)
import qualified Text.Pandoc.Options as Opt

applyDefaultTemplate :: Context String -> Item String -> Compiler (Item String)
applyDefaultTemplate = loadAndApplyTemplate "templates/default.html"

applySkeletonTemplate :: Context String -> Item String -> Compiler (Item String)
applySkeletonTemplate = loadAndApplyTemplate "templates/skeleton.html"

applyPageTemplate :: Context String -> Item String -> Compiler (Item String)
applyPageTemplate baseCtx =
  foldl (>=>) pure $ templates <*> [baseCtx]
  where
    templates = [applyDefaultTemplate, applySkeletonTemplate]

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
