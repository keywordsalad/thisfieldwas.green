module Site.Compiler.Layout where

import Control.Monad ((<=<))
import Data.Binary as B
import Data.ByteString.Lazy as LBS
import GHC.Generics hiding (to)
import Hakyll
import Lens.Micro
import Lens.Micro.TH
import Site.Config

data Layout = Layout
  { _layoutStack :: [Item Template],
    _layoutScripts :: [Item String],
    _layoutStylesheets :: [Item String]
  }
  deriving stock (Generic)

makeLenses ''Layout

instance Binary Layout where
  get = Layout <$> get <*> get <*> get
  put layout =
    put (layout ^. layoutStack)
      >> put (layout ^. layoutScripts)
      >> put (layout ^. layoutStylesheets)

instance Writable Layout where
  write p = LBS.writeFile p . B.encode . itemBody

layoutKey :: String
layoutKey = "layout"

layoutScriptsKey :: String
layoutScriptsKey = "scripts"

layoutStylesheetsKey :: String
layoutStylesheetsKey = "stylesheets"

layoutContext :: SimpleGetter Layout (Context String)
layoutContext = to \layout ->
  listField layoutScriptsKey context (return $ layout ^. layoutScripts)
    <> listField layoutStylesheetsKey context (return $ layout ^. layoutStylesheets)
  where
    context = bodyField "src"

loadLayoutFromMetadata :: Metadata -> Compiler (Maybe (Item Layout))
loadLayoutFromMetadata metadata =
  mapM (loadLayout . fromLayoutName) (lookupString layoutKey metadata)

applyLayoutFromMetadata :: SiteConfig -> Item String -> Compiler (Item String)
applyLayoutFromMetadata config item = do
  metadata <- getMetadata $ itemIdentifier item
  maybeLayout <- loadLayoutFromMetadata metadata
  let f layout = applyLayout config layout item
  maybe (return item) f maybeLayout

applyLayout :: SiteConfig -> Item Layout -> Item String -> Compiler (Item String)
applyLayout config layout = go templates
  where
    templates = itemBody <$> layout ^. to itemBody . layoutStack
    context = bodyField "body" <> config ^. siteContext
    go (t : ts) = go ts <=< applyTemplate t context
    go [] = return

loadLayout :: Identifier -> Compiler (Item Layout)
loadLayout = load

layoutCompiler :: Compiler (Item Layout)
layoutCompiler = do
  metadata <- getMetadata =<< getUnderlying
  template <- makeItem =<< compileTemplateItem =<< getResourceBody
  parent <- loadLayoutFromMetadata metadata

  let parentScripts = parentItems layoutScripts parent
      scripts = parentScripts ++ toUrlItems (lookupStringList layoutScriptsKey metadata)

      parentStylesheets = parentItems layoutStylesheets parent
      stylesheets = parentStylesheets ++ toUrlItems (lookupStringList layoutStylesheetsKey metadata)

      parentStack = parentItems layoutStack parent
      stack = template : parentStack

  makeItem
    Layout
      { _layoutStack = stack,
        _layoutScripts = scripts,
        _layoutStylesheets = stylesheets
      }
  where
    toUrlItems = maybe [] (fmap toUrlItem)
    toUrlItem filePath = Item (fromFilePath filePath) (toUrl filePath)
    parentItems lens' = maybe [] (^. to itemBody . lens')

fromLayoutName :: String -> Identifier
fromLayoutName name = fromFilePath ("layouts/" ++ name ++ ".html")
