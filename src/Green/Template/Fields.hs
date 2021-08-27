{-# LANGUAGE AllowAmbiguousTypes #-}

module Green.Template.Fields where

import Control.Monad (forM)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import Data.String.Utils (endswith)
import qualified Data.Text as T
import Green.Template
import Green.Util (stripSuffix)
import Hakyll (Compiler, Item (..))
import qualified Hakyll as H
import System.FilePath

defaultFields :: Context String
defaultFields =
  mconcat
    [ routeField,
      linkedTitleField,
      bodyField "body",
      urlField "url",
      pathField "path",
      ifField,
      forField,
      defaultField,
      titleFromFileField "title",
      withField,
      undefinedField
    ]

undefinedField :: Context a
undefinedField = Context f
  where
    f k _ = return $ UndefinedValue k

defaultKeys :: [String] -> Context a
defaultKeys keys = intoContext $ (,"" :: String) <$> keys

withField :: Context String
withField = functionField2 "with" f
  where
    f ::
      FunctionValue2
        (Context String)
        (FunctionValue [Block] String String)
        (FunctionValue [Block] String String)
        String
    f context' g _ _ = do
      return \t context -> g t (context' <> context)

includeField :: String -> FilePath -> Context String
includeField key basePath = functionField key f
  where
    f (filePath :: String) =
      loadAndApplyTemplate $
        H.fromFilePath (basePath </> filePath <.> "html")

layoutField :: String -> FilePath -> Context String
layoutField key basePath = functionField2 key f
  where
    f (filePath :: FilePath) (blocks :: [Block]) context item = do
      let layoutId = H.fromFilePath $ basePath </> filePath <.> "html"
      reduced <- reduceBlocks context blocks item
      template <- loadTemplateBody layoutId
      applyTemplate template context (H.itemSetBody reduced item)

ifField :: forall a. Context a
ifField = functionField2 "if" f
  where
    f (arg :: ContextValue a) (blocks :: [Block]) _ _ =
      isTruthy arg <&> \case
        True -> Just blocks
        False -> Nothing

forField :: Context String
forField = functionField2 "for" f
  where
    f (arg :: ContextValue String) (blocks :: [Block]) context item = do
      arg' <- force arg
      isTruthy arg' >>= \case
        True ->
          case arg' of
            ListValue xs -> do
              ctxs <- sequence $ fromValue <$> xs :: Compiler [Context String]
              Just . mconcat <$> forM ctxs reduce
            ContextValue ctx -> do
              Just <$> reduce ctx
            x -> fail $ "Unexpected " ++ show x ++ " in {{#for}}"
        False -> return Nothing
      where
        reduce x = reduceBlocks (x <> context) blocks item

defaultField :: forall a. Context a
defaultField = functionField2 "default" f
  where
    f (default' :: ContextValue a) (arg :: ContextValue a) _ _ =
      isTruthy arg <&> \case
        True -> arg
        False -> default'

routeField :: Context String
routeField = functionField "route" f
  where
    f (filePath :: String) _ _ = do
      let id' = H.fromFilePath filePath
      H.getRoute id' >>= \case
        Just r -> return $ "/" ++ stripSuffix "index.html" r
        Nothing -> error $ "no route to " ++ show id'

linkedTitleField :: Context String
linkedTitleField = constField "linkedTitle" f
  where
    f :: FunctionValue String String String
    f filePath context item = do
      linkedItem <- H.load (H.fromFilePath filePath)
      makeLink <$> getField "title" linkedItem <*> getField "url" linkedItem
      where
        getField key linkedItem = tryWithError key item $ fromValue =<< unContext context key linkedItem
        makeLink title url
          | endswith ".html" filePath = "<a href=\"" ++ url ++ "\">" ++ H.escapeHtml title ++ "</a>"
          | endswith ".md" filePath = "[" ++ H.escapeHtml title ++ "](" ++ url ++ ")"
          | otherwise = title ++ " <" ++ url ++ ">"

metadataField :: forall a. Context a
metadataField = Context f
  where
    f :: ContextFunction a
    f key item = do
      m <- H.getMetadata (itemIdentifier item)
      maybe
        (fail $ "Key " ++ show key ++ " not found in metadata")
        (return . intoValue)
        (HashMap.lookup (T.pack key) m)

bodyField :: String -> Context String
bodyField key = field key $ return . itemBody

urlField :: String -> Context a
urlField key = field key f
  where
    f item =
      let id' = itemIdentifier item
          empty' = fail $ "No route url found for item " ++ show id'
       in maybe empty' H.toUrl <$> H.getRoute id'

pathField :: String -> Context a
pathField key = field key $ return . H.toFilePath . itemIdentifier

titleFromFileField :: String -> Context a
titleFromFileField = bindField titleFromPath . pathField
  where
    titleFromPath = return . takeBaseName

teaserField :: String -> H.Snapshot -> Context String
teaserField key snapshot = field key f
  where
    f item =
      takeTeaser "" <$> H.loadSnapshotBody (itemIdentifier item) snapshot

    teaserComment = "<!-- teaser -->"
    takeTeaser acc body@(x : rest)
      | body `startsWith` teaserComment = reverse acc
      | otherwise = takeTeaser (x : acc) rest
    takeTeaser _ [] = ""

    startsWith (x : xs) (y : ys)
      | x == y = startsWith xs ys
      | otherwise = False
    startsWith _ [] = True
    startsWith [] _ = False
