{-# LANGUAGE AllowAmbiguousTypes #-}

module Green.Template.Field where

import qualified Data.HashMap.Strict as HashMap
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
      routeField "route",
      putField "put",
      linkedTitleField,
      metadataPriorityField "updated" ["updated", "published", "created"],
      metadataPriorityField "published" ["updated", "created"],
      ifField,
      forField,
      defaultField,
      withField "with",
      metadataField,
      titleFromFileField "title",
      missingField
    ]

emptyString :: ContextValue a
emptyString = intoValue ("" :: String)

defaultKeys :: [String] -> Context a
defaultKeys keys = intoContext $ (,"" :: String) <$> keys

withField :: String -> Context String
withField key = functionField2 key f
  where
    f (context :: Context String) (blocks :: [Block]) =
      tplWithContext context do
        reduceBlocks blocks

includeField :: String -> FilePath -> Context String
includeField key basePath = functionField key f
  where
    f (filePath :: String) = do
      let filePath' = basePath </> filePath <.> "html"
      tplWithCall (filePath' ++ " via " ++ show key) do
        context <- tplContext
        applyTemplate (fromFilePath filePath')
        itemValue context <$> tplPopItem

layoutField :: String -> FilePath -> Context String
layoutField key basePath = functionField2 key f
  where
    f (filePath :: FilePath) (content :: String) = do
      let filePath' = basePath </> filePath <.> "html"
      tplWithCall (filePath' ++ " via " ++ show key) do
        let layoutId = fromFilePath filePath'
        (Template bs _) <- loadTemplate layoutId
        item <- itemSetBody content <$> tplItem
        tplWithItem item do
          reduceBlocks bs

ifField :: forall a. Context a
ifField = functionField "if" isTruthy

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

routeField :: String -> Context String
routeField key = functionField key f
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
          | ".html" `isSuffixOf` filePath = "<a href=\"" ++ url ++ "\">" ++ escapeHtml title ++ "</a>"
          | ".md" `isSuffixOf` filePath = "[" ++ escapeHtml title ++ "](" ++ url ++ ")"
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
        (return . ("/" ++))
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
    f item = lift do
      body <- loadSnapshotBody (itemIdentifier item) snapshot
      case takeTeaser body of
        Just teaser -> return teaser
        Nothing -> fail $ "item " ++ itemFilePath item ++ " has no teaser"
    takeTeaser = go ""
      where
        go acc xss@(x : xs)
          | "<!--more-->" `isPrefixOf` xss = Just (reverse acc)
          | otherwise = go (x : acc) xs
        go _ [] = Nothing

metadataPriorityField :: String -> [String] -> Context a
metadataPriorityField key priorityKeys = field key f
  where
    f item = lift $ foldl (<|>) (noResult "") (flip getMetadataField item <$> priorityKeys)

putField :: forall a. String -> Context a
putField key = functionField key tplPut

putBlockField :: String -> Context a
putBlockField key = functionField2 key f
  where
    f (name :: String) (blocks :: [Block]) = do
      tplPut $ constField name blocks
