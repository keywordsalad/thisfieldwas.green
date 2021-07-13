module Green.Compiler.Layout where

import Data.Binary as B
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics
import Green.Common
import Green.Config
import Green.Util

layoutCompiler :: Compiler (Item Layout)
layoutCompiler = do
  template <- templateBodyCompiler
  fields <- layoutMetadataCompiler =<< getUnderlying
  makeItem $ Layout template fields

applyLayout :: SiteConfig -> Item String -> Compiler (Item String)
applyLayout config item = do
  metadata <- layoutMetadataCompiler (itemIdentifier item)
  stack <- buildStack (metadataParent metadata) (stackFromMetadata metadata)
  let context = stackContext stack <> config ^. siteContext
  debugCompiler $
    "[LayoutStack " ++ toFilePath (itemIdentifier item) ++ "]\n"
      ++ unlines
        [ "templates   => " ++ commas (toFilePath . itemIdentifier <$> stackTemplates stack),
          "body-class  => " ++ commas (stackBodyClasses stack),
          "scripts     => " ++ commas (itemBody <$> stackScripts stack),
          "stylesheets => " ++ commas (itemBody <$> stackStylesheets stack)
        ]
  go (itemBody <$> stackTemplates stack) context item
  where
    go (template : rest) context body = go rest context =<< applyTemplate template context body
    go [] _ body = return body
    buildStack Nothing stack = return stack
    buildStack (Just parentId) stack = do
      layout <- itemBody <$> loadLayout parentId
      buildStack (layoutParent layout) (stackAppendLayout layout stack)
    layoutParent = metadataParent . layoutMetadata

loadLayout :: Identifier -> Compiler (Item Layout)
loadLayout = load

data Layout = Layout
  { layoutTemplate :: Item Template,
    layoutMetadata :: LayoutMetadata
  }
  deriving stock (Show, Generic)

instance Binary Layout where
  get = Layout <$> get <*> get
  put layout =
    put (layoutTemplate layout)
      >> put (layoutMetadata layout)

instance Writable Layout where
  write p = LBS.writeFile p . B.encode . itemBody

data LayoutMetadata = LayoutMetadata
  { metadataParent :: Maybe Identifier,
    metadataBodyClasses :: [String],
    metadataScripts :: [Item String],
    metadataStylesheets :: [Item String]
  }
  deriving stock (Show, Generic)

instance Binary LayoutMetadata where
  get = LayoutMetadata <$> get <*> get <*> get <*> get
  put metadata =
    put (metadataParent metadata)
      >> put (metadataBodyClasses metadata)
      >> put (metadataScripts metadata)
      >> put (metadataStylesheets metadata)

layoutMetadataCompiler :: Identifier -> Compiler LayoutMetadata
layoutMetadataCompiler id' = do
  metadata <- getMetadata id'
  let listFromKeys keys = fromMaybe [] $ firstMaybe $ [flip lookupStringList metadata, fmap pure . flip lookupString metadata] <*> keys
      scripts = toUrlItems $ listFromKeys ["scripts", "script"]
      stylesheets = toUrlItems $ listFromKeys ["stylesheets", "stylesheet"]
      bodyClasses = listFromKeys ["body-classes", "body-class"]
      parentId = fromLayoutName <$> lookupString "layout" metadata
      fields =
        LayoutMetadata
          { metadataParent = parentId,
            metadataBodyClasses = bodyClasses,
            metadataScripts = scripts,
            metadataStylesheets = stylesheets
          }
  debugCompiler $
    "[LayoutMetadata " ++ toFilePath id' ++ "]\n"
      ++ unlines
        [ "layout       => " ++ maybe "ROOT" show parentId,
          "body-class   => " ++ commas bodyClasses,
          "scripts      => " ++ commas (itemBody <$> scripts),
          "stylesheets  => " ++ commas (itemBody <$> stylesheets)
        ]
  return fields
  where
    toUrlItems = fmap toUrlItem
    toUrlItem filePath = Item (fromFilePath filePath) (toUrl filePath)
    fromLayoutName name = fromFilePath $ "_layouts/" ++ name ++ ".html"

data LayoutStack = LayoutStack
  { stackTemplates :: [Item Template],
    stackBodyClasses :: [String],
    stackScripts :: [Item String],
    stackStylesheets :: [Item String]
  }

stackFromMetadata :: LayoutMetadata -> LayoutStack
stackFromMetadata metadata =
  LayoutStack
    { stackTemplates = mempty,
      stackBodyClasses = metadataBodyClasses metadata,
      stackScripts = metadataScripts metadata,
      stackStylesheets = metadataStylesheets metadata
    }

stackAppendLayout :: Layout -> LayoutStack -> LayoutStack
stackAppendLayout layout stack =
  let LayoutStack templates bodyClasses scripts stylesheets = stack
      Layout template (LayoutMetadata _ mdBodyClasses mdScripts mdStylesheets) = layout
   in LayoutStack
        (templates <> pure template)
        (bodyClasses <> mdBodyClasses)
        (scripts <> mdScripts)
        (stylesheets <> mdStylesheets)

stackContext :: LayoutStack -> Context String
stackContext (LayoutStack _ bodyClasses scripts stylesheets) =
  mconcat
    [ constField "bodyClass" (unwords bodyClasses),
      listField "scripts" (bodyField "src") (return scripts),
      listField "stylesheets" (bodyField "href") (return stylesheets)
    ]
