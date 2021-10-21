{-# LANGUAGE AllowAmbiguousTypes #-}

module Green.Template.Field where

import qualified Data.HashMap.Strict as HashMap
import Data.String.Utils (endswith)
import qualified Data.Text as T
import Green.Common
import Green.Template.Ast
import Green.Template.Compiler
import Green.Template.Context
import Green.Util (stripSuffix)
import System.FilePath

defaultFields :: Context String
defaultFields =
  mconcat
    [ bodyField "body",
      urlField "url",
      pathField "path",
      routeField,
      linkedTitleField,
      ifField,
      forField,
      defaultField,
      withField,
      metadataField,
      titleFromFileField "title",
      missingField
    ]

defaultKeys :: [String] -> Context a
defaultKeys keys = intoContext $ (,"" :: String) <$> keys

withField :: Context String
withField = functionField2 "with" f
  where
    f ::
      FunctionValue2
        (Context String)
        [Block]
        String
        String
    f context blocks =
      tplWithContext context do
        reduceBlocks blocks

includeField :: String -> FilePath -> Context String
includeField key basePath = functionField key f
  where
    f (filePath :: String) = do
      let filePath' = basePath </> filePath <.> "html"
      tplWithCall (filePath' ++ " included via " ++ show key) do
        context <- tplContext
        result <- loadAndApplyTemplate' (fromFilePath filePath')
        return $ itemValue context result

layoutField :: String -> FilePath -> Context String
layoutField key basePath = functionField2 key f
  where
    f (filePath :: FilePath) (blocks :: [Block]) = do
      let filePath' = basePath </> filePath <.> "html"
      tplWithCall (filePath' ++ " applied via " ++ show key) do
        let layoutId = fromFilePath filePath'
        (Template bs _) <- loadTemplate' layoutId
        item <- itemSetBody <$> reduceBlocks blocks <*> tplItem
        tplWithItem item do
          reduceBlocks bs

ifField :: forall a. Context a
ifField = functionField2 "if" f
  where
    f (arg :: ContextValue a) (blocks :: [Block]) =
      isTruthy arg <&> \case
        True -> Just blocks
        False -> Nothing

forField :: Context String
forField = functionField2 "for" f
  where
    f ((context, items) :: (Context String, [Item String])) (blocks :: [Block])
      | null items = return Nothing
      | otherwise =
        tplWithContext context do
          Just . mconcat <$> forM items \item ->
            tplWithItem item do
              reduceBlocks blocks

defaultField :: forall a. Context a
defaultField = functionField2 "default" f
  where
    f (default' :: ContextValue a) (arg :: ContextValue a) =
      isTruthy arg <&> \case
        True -> arg
        False -> default'

routeField :: Context String
routeField = functionField "route" f
  where
    f (filePath :: String) = lift do
      let id' = fromFilePath filePath
      getRoute id' >>= \case
        Just r -> return $ "/" ++ stripSuffix "index.html" r
        Nothing -> noResult $ "no route to " ++ show id'

linkedTitleField :: Context String
linkedTitleField = constField "linkedTitle" f
  where
    f :: FunctionValue String String String
    f filePath = do
      linkedItem <- lift $ load (fromFilePath filePath)
      tplWithItem linkedItem do
        makeLink <$> getField "title" <*> getField "url"
      where
        getField key = do
          context <- tplContext
          fromValue =<< unContext context key
        makeLink title url
          | endswith ".html" filePath = "<a href=\"" ++ url ++ "\">" ++ escapeHtml title ++ "</a>"
          | endswith ".md" filePath = "[" ++ escapeHtml title ++ "](" ++ url ++ ")"
          | otherwise = title ++ " <" ++ url ++ ">"

metadataField :: forall a. Context a
metadataField = Context f
  where
    f key = lift . getMetadataField key =<< tplItem

getMetadataField :: String -> Item a -> Compiler (ContextValue a)
getMetadataField key item = do
  m <- getMetadata (itemIdentifier item)
  maybe
    (noResult $ "tried metadata key " ++ show key)
    (return . intoValue)
    (HashMap.lookup (T.pack key) m)

bodyField :: String -> Context String
bodyField key = field key $ return . itemBody

urlField :: String -> Context a
urlField key = field key f
  where
    f item = lift do
      let id' = itemIdentifier item
      maybeRoute <- getRoute id'
      maybe
        (fail $ "no url by " ++ show key ++ " found for item " ++ show id')
        return
        maybeRoute

pathField :: String -> Context a
pathField key = field key $ return . toFilePath . itemIdentifier

titleFromFileField :: String -> Context a
titleFromFileField = bindField titleFromPath . pathField
  where
    titleFromPath = return . takeBaseName

teaserField :: String -> Snapshot -> Context String
teaserField key snapshot = field key f
  where
    f item =
      lift $ takeTeaser "" <$> loadSnapshotBody (itemIdentifier item) snapshot

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
