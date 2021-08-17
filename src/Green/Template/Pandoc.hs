module Green.Template.Pandoc where

import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Hakyll (Compiler, Item)
import qualified Hakyll as H
import System.FilePath
import Text.Pandoc hiding (readers, writers)

readers :: (PandocMonad m) => ReaderOptions -> HashMap String (String -> m Pandoc)
readers opts =
  HashMap.fromList $
    second f
      <$> [ ("md", readMarkdown),
            ("markdown", readMarkdown)
          ]
  where
    f reader = reader opts . T.pack

writers :: (PandocMonad m) => WriterOptions -> HashMap String (Pandoc -> m String)
writers opts =
  HashMap.fromList $
    second f
      <$> [ ("html", writeHtml5String)
          ]
  where
    f writer = fmap T.unpack . writer opts

compilePandoc :: Item String -> Compiler (Item String)
compilePandoc item = do
  let id' = H.itemIdentifier item
      srcExt = takeExtension $ H.toFilePath id'
  let readerLookup = HashMap.lookup srcExt (readers def)
  reader <- maybe (fail $ "Unrecognized extension " ++ show srcExt) return readerLookup

  destExt <- maybe (fail $ "No route found for " ++ show id') (return . takeExtension) =<< H.getRoute id'
  let writerLookup = HashMap.lookup destExt (writers def)
  writer <- maybe (fail $ "Unrecognized extension " ++ show destExt) return writerLookup

  case runPure . writer =<< runPure (reader $ H.itemBody item) of
    Right x -> H.makeItem x
    Left e -> fail $ show e
