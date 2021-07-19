module Green.Template.Layout where

import Control.Applicative
import Data.Bifunctor
import Data.Binary as B
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import Green.Template.Compiler
import Green.Template.Context
import Green.Template.Data
import Hakyll hiding (Context, Template, applyTemplate, fromList, lookupStringList, templateBodyCompiler, templateCompiler)
import Prelude hiding (lookup)

layoutCompiler :: Compiler (Item Layout)
layoutCompiler = do
  template <- templateCompiler
  fields <- layoutMetadataCompiler =<< getUnderlying
  makeItem $ Layout template fields

applyLayout :: Context -> Item String -> Compiler (Item String)
applyLayout context item = do
  metadata <- layoutMetadataCompiler (itemIdentifier item)
  stack <- buildStack (metadataParent metadata) (stackFromMetadata metadata)
  let context' = context <> stackContext stack
  debugCompiler $
    "[LayoutStack " ++ toFilePath (itemIdentifier item) ++ "]\n"
      ++ unlines
        [ "templates   => " ++ show (toFilePath . itemIdentifier <$> stackTemplates stack),
          "body-class  => " ++ show (stackBodyClasses stack),
          "scripts     => " ++ show (stackScripts stack),
          "stylesheets => " ++ show (stackStylesheets stack),
          "context     => " ++ show context'
        ]
  let go (template : rest) body = go rest =<< applyTemplate context' body
      go [] body = return body
   in go (itemBody <$> stackTemplates stack) item
  where
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
    metadataBodyClasses :: [T.Text],
    metadataScripts :: [T.Text],
    metadataStylesheets :: [T.Text]
  }
  deriving stock (Show, Generic)

instance Binary LayoutMetadata where
  get = LayoutMetadata <$> get <*> get <*> get <*> get
  put metadata =
    put (metadataParent metadata)
      >> put (metadataBodyClasses metadata)
      >> put (metadataScripts metadata)
      >> put (metadataStylesheets metadata)

parentKey :: T.Text
parentKey = T.pack "layout"

bodyClassKeys :: [T.Text]
bodyClassKeys = T.pack <$> ["body-class", "body-classes", "bodyClass", "bodyClasses"]

stylesheetKeys :: [T.Text]
stylesheetKeys = T.pack <$> ["stylesheet", "stylesheets"]

scriptKeys :: [T.Text]
scriptKeys = T.pack <$> ["script", "scripts"]

layoutMetadataCompiler :: Identifier -> Compiler LayoutMetadata
layoutMetadataCompiler id' = do
  context <- getContext id'
  let parentId = fromLayoutName <$> lookupText parentKey context
      bodyClasses =
        fmap fromJust $
          sequence $
            pick (Just [T.pack "default"]) $
              flip lookupTextList context <$> bodyClassKeys
      scripts = fromJust $ pick (Just []) $ flip lookupTextList context <$> scriptKeys
      stylesheets = fromJust $ pick (Just []) $ flip lookupTextList context <$> stylesheetKeys
      metadata =
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
          "body-class   => " ++ show bodyClasses,
          "scripts      => " ++ show scripts,
          "stylesheets  => " ++ show stylesheets
        ]
  return metadata
  where
    fromLayoutName name = fromFilePath ("_layouts/" ++ T.unpack name ++ ".html")
    pick default' maybes = foldl (<|>) Nothing maybes <|> default'

data LayoutStack = LayoutStack
  { stackTemplates :: [Item Template],
    stackBodyClasses :: [T.Text],
    stackScripts :: [T.Text],
    stackStylesheets :: [T.Text]
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
        (templates <> [template])
        (bodyClasses <> mdBodyClasses)
        (scripts <> mdScripts)
        (stylesheets <> mdStylesheets)

stackContext :: LayoutStack -> Context
stackContext (LayoutStack _ bodyClasses scripts stylesheets) =
  fromList $
    first T.pack
      <$> [ ("bodyClass", TextValue (T.unwords bodyClasses)),
            ("scripts", attrContext "src" scripts),
            ("stylesheets", attrContext "href" stylesheets)
          ]
  where
    attrContext key values =
      ListValue $
        ContextValue . singleton (T.pack key) . TextValue <$> values
