{-# LANGUAGE AllowAmbiguousTypes #-}

module Green.Hakyllbars.Field
  ( module Green.Hakyllbars.Field.Date,
    module Green.Hakyllbars.Field.Git,
    module Green.Hakyllbars.Field.Html,
    defaultFields,
    emptyString,
    defaultKeys,
    includeField,
    layoutField,
    ifField,
    forField,
    withField,
    forEachField,
    defaultField,
    linkedTitleField,
    metadataField,
    siteUrlField,
    urlField,
    absUrlField,
    getUrlField,
    getAbsUrlField,
    titleFromFileField,
    teaserField,
    metadataPriorityField,
    namedMetadataField,
    putField,
    addField,
    putBlockField,
    addBlockField,
  )
where

import Control.Monad.Except
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Green.Hakyllbars.Ast
import Green.Hakyllbars.Common
import Green.Hakyllbars.Compiler
import Green.Hakyllbars.Context
import Green.Hakyllbars.Field.Date (DateConfig, dateFields, defaultDateConfigWith)
import Green.Hakyllbars.Field.Git (gitFields)
import Green.Hakyllbars.Field.Html (escapeHtmlField, escapeHtmlUriField)
import Green.Hakyllbars.Util (stripSuffix)
import System.FilePath

-- | The default recommended fields to use for your website templates.
defaultFields :: String -> String -> Context String
defaultFields host siteRoot =
  mconcat
    [ bodyField "body",
      constField "host" host,
      constField "siteRoot" siteRoot,
      pathField "path",
      siteUrlField "siteUrl" "host" "siteRoot",
      urlField "url" "siteRoot",
      absUrlField "absUrl" "host" "url",
      getUrlField "getUrl" "siteRoot",
      getAbsUrlField "getAbsUrl" "host" "getUrl",
      linkedTitleField "linkedTitle" "title" "url",
      escapeHtmlField,
      escapeHtmlUriField,
      putField "put",
      addField "add",
      putBlockField "putBlock",
      addBlockField "addBlock",
      ifField "if",
      forField "for",
      defaultField "default",
      withField "with",
      includeField "include" Nothing Nothing,
      includeField "partial" (Just "_partials") (Just "html"),
      layoutField "applyLayout" "_layouts" (Just "html"),
      metadataPriorityField "updated" ["updated", "published", "created"],
      metadataPriorityField "published" ["published", "created"],
      metadataField,
      titleFromFileField "title",
      constField "description" ("" :: String)
    ]

-- | An empty string context value.
emptyString :: ContextValue a
emptyString = intoValue ("" :: String)

-- | A context with the given keys and empty string values.
defaultKeys :: [String] -> Context a
defaultKeys keys = intoContext $ (,"" :: String) <$> keys

-- | Sets a scope in which the given fields are active in the context.
withField :: String -> Context String
withField key = functionField2 key f
  where
    f (context :: Context String) (blocks :: [Block]) =
      tplWithContext context do
        reduceBlocks blocks

-- | Includes the given file in the template.
includeField :: String -> Maybe FilePath -> Maybe FilePath -> Context String
includeField key basePath extension = functionField key f
  where
    f (filePath :: String) = do
      basePath' <- maybe (itemFilePath <$> tplItem) return basePath
      let filePath' = basePath' </> filePath
      let filePath'' = maybe filePath' (filePath' <.>) extension
      context <- tplContext
      applyTemplate (fromFilePath filePath'')
      itemValue context <$> tplPopItem

-- | Sets a layout to interpolate the template into.
layoutField :: String -> FilePath -> Maybe FilePath -> Context String
layoutField key basePath extension = functionField2 key f
  where
    f (filePath :: FilePath) (content :: String) = do
      let filePath' = basePath </> filePath
      let filePath'' = maybe filePath' (filePath' <.>) extension
      let layoutId = fromFilePath filePath''
      (Template bs _) <- loadTemplate layoutId
      item <- itemSetBody content <$> tplItem
      tplWithItem item do
        reduceBlocks bs

-- | Conditionally renders a block.
ifField :: forall a. String -> Context a
ifField key = functionField key isTruthy

-- | Context field for iterating over a list of items.
forField :: String -> Context String
forField key = functionField2 key applyForLoop

-- | Iterates over a list of items, applying their context to the given block.
applyForLoop :: ContextValue String -> [Block] -> TemplateRunner String (Maybe String)
applyForLoop items blocks =
  getAsItems items
    `catchError` (\_ -> getAsStrings items)
    `catchError` (\_ -> return (mempty, []))
    >>= uncurry go
  where
    go context items'
      | null items' = return Nothing
      | otherwise = tplWithContext context do
          Just . mconcat <$> forM items' \item ->
            tplWithItem item do
              reduceBlocks blocks

-- | Gets a context value as a list of items.
getAsItems :: ContextValue String -> TemplateRunner String (Context String, [Item String])
getAsItems = fromValue

-- | Gets a context value as a list of strings.
getAsStrings :: ContextValue String -> TemplateRunner String (Context String, [Item String])
getAsStrings x = do
  bodies <- fromValue x :: TemplateRunner String [String]
  items <- forM bodies \body -> itemSetBody body <$> tplItem
  return (bodyField "item", items)

forEachField :: String -> Context String
forEachField key = functionField3 key f
  where
    f (forEachKey :: ContextValue String) (forEachItems :: ContextValue String) (blocks :: [Block]) = do
      keyId <- getKey forEachKey
      keyItemPairs <- fromValue forEachItems :: TemplateRunner String [(ContextValue String, ContextValue String)]
      keyItemPairs `forM` \(key', items) ->
        tplWithContext (constField keyId key') do
          applyForLoop items blocks
    getKey block = case block of
      UndefinedValue k _ _ _ -> return k -- allow identifier as key
      StringValue k -> return k
      _ -> tplFail "forEach: key must be a string or identifier"

-- | Gets a default context value if none is provided.
defaultField :: forall a. String -> Context a
defaultField key = functionField2 key f
  where
    f (default' :: ContextValue a) (arg :: ContextValue a) =
      isTruthy arg <&> \case
        True -> arg
        False -> default'

-- | Creates a link with the title to the given item.
linkedTitleField :: String -> String -> String -> Context String
linkedTitleField key titleKey urlKey = constField key f
  where
    f :: FunctionValue String String String
    f filePath = do
      tplWithItem (Item (fromFilePath filePath) "") do
        makeLink <$> getField titleKey <*> getField urlKey
      where
        getField key' = do
          context <- tplContext
          fromValue =<< unContext context key'
        makeLink title url
          | ".html" `isSuffixOf` filePath = "<a href=\"" ++ escapeHtml url ++ "\" title=\"" ++ escapeHtml title ++ "\">" ++ escapeHtml title ++ "</a>"
          | ".md" `isSuffixOf` filePath = "[" ++ title ++ "](" ++ url ++ " \"" ++ title ++ "\")"
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
    (KeyMap.lookup (Key.fromString key) m)

-- | The body of the current item.
bodyField :: String -> Context String
bodyField key = field key $ return . itemBody

-- | The absolute url to the site root.
siteUrlField :: String -> String -> String -> Context a
siteUrlField key hostKey siteRootKey = field key f
  where
    f _ = do
      context <- tplContext
      host <- fromValue =<< unContext context hostKey
      siteRoot <- fromValue =<< unContext context siteRootKey
      return (host ++ siteRoot :: String)

-- | The url path to the given item.
urlField :: String -> String -> Context a
urlField key siteRootKey = field key f
  where
    f = getUri key siteRootKey . itemIdentifier

-- | Gets the url path to the given item file path.
getUrlField :: String -> String -> Context a
getUrlField key siteRootKey = functionField key f
  where
    f = getUri key siteRootKey . fromFilePath

-- | Gets the uri to the given item identifier.
getUri :: String -> String -> Identifier -> TemplateRunner a String
getUri key siteRootKey id' = do
  siteRoot <-
    tplContext
      >>= flip unContext siteRootKey
      >>= fromValue
  maybeRoute <- lift $ getRoute id'
  definitelyRoute <-
    maybe
      (fail $ "no route by " ++ show key ++ " found for item " ++ show id')
      (return . ("/" ++))
      maybeRoute
  let uri = stripSuffix "index.html" definitelyRoute
  return if null uri then siteRoot else siteRoot ++ uri

-- | Gets the absolute url to the current item.
absUrlField :: String -> String -> String -> Context a
absUrlField key hostKey urlKey = field key f
  where
    f _ = do
      context <- tplContext
      host <- fromValue =<< unContext context hostKey
      url <- fromValue =<< unContext context urlKey
      return (host ++ url :: String)

-- | Gets the absolute url to the given item file path.
getAbsUrlField :: forall a. String -> String -> String -> Context a
getAbsUrlField key hostKey getUrlKey = functionField key f
  where
    f (filePath :: FilePath) = do
      context <- tplContext
      host <- fromValue =<< unContext context hostKey
      getUrl <- fromValue =<< unContext context getUrlKey
      url <- getUrl (intoValue filePath :: ContextValue a)
      return (host ++ url :: String)

-- | Gets the destination path to the current item.
pathField :: String -> Context a
pathField key = field key $ return . toFilePath . itemIdentifier

-- | Gets the title of the current item from the file name.
titleFromFileField :: String -> Context a
titleFromFileField = bindField titleFromPath . pathField
  where
    titleFromPath = return . takeBaseName

-- | Extracts the teaser from the current item.
--
-- The teaser is noted in the item body with the HTML comment `<!--more-->`. All
-- content preceding this comment is considered the teaser.
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

-- | Gets the value of the first metadata key that exists.
metadataPriorityField ::
  -- | The context key.
  String ->
  -- | The list of metadata keys to try in order of priority.
  [String] ->
  Context a
metadataPriorityField key priorityKeys = field key f
  where
    f item =
      lift $
        foldl
          (<|>)
          (noResult $ "Metadata priority key " ++ show key ++ " from set " ++ show priorityKeys)
          (flip getMetadataField item <$> priorityKeys)

namedMetadataField :: String -> Context String
namedMetadataField key = field key $ lift . getMetadataField key

putField :: String -> Context a
putField key = functionField key tplPut

addField :: forall a. String -> Context a
addField key = functionField2 key f
  where
    f (name :: String) (value :: ContextValue a) = do
      current <- tplGet name `catchError` \_ -> return []
      tplPut $ constField name (value : current)

-- | Puts a block of content into the context by a given name.
putBlockField :: String -> Context a
putBlockField key = functionField2 key f
  where
    f (name :: String) (blocks :: [Block]) = do
      tplPut $ constField name blocks

-- | Adds a block of content to the given context collection identified by a name.
addBlockField :: String -> Context a
addBlockField key = functionField2 key f
  where
    f (name :: String) (blocks :: [Block]) = do
      current <- tplGet name `catchError` \_ -> return []
      tplPut $ constField name (current ++ blocks)
