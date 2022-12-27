{-# LANGUAGE AllowAmbiguousTypes #-}

module Green.Template.Field where

import Control.Monad.Except
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
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
      absUrlField "absUrl" "siteRoot" "url",
      pathField "path",
      getUrlField "getUrl",
      getAbsUrlField "getAbsUrl" "siteRoot" "getUrl",
      putField "put",
      addField "add",
      putBlockField "putBlock",
      addBlockField "addBlock",
      linkedTitleField,
      metadataPriorityField "updated" ["updated", "published", "created"],
      metadataPriorityField "published" ["updated", "created"],
      ifField,
      forField,
      defaultField,
      withField "with",
      metadataField,
      titleFromFileField "title",
      constField "description" ("" :: String)
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

includeField :: String -> Maybe FilePath -> Context String
includeField key basePath = functionField key f
  where
    f (filePath :: String) = do
      let filePath' = maybe filePath (</> filePath) basePath <.> "html"
      context <- tplContext
      applyTemplate (fromFilePath filePath')
      itemValue context <$> tplPopItem

layoutField :: String -> FilePath -> Context String
layoutField key basePath = functionField2 key f
  where
    f (filePath :: FilePath) (content :: String) = do
      let filePath' = basePath </> filePath <.> "html"
      let layoutId = fromFilePath filePath'
      (Template bs _) <- loadTemplate layoutId
      item <- itemSetBody content <$> tplItem
      tplWithItem item do
        reduceBlocks bs

ifField :: forall a. Context a
ifField = functionField "if" isTruthy

forField :: Context String
forField = functionField2 "for" applyForLoop

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

getAsItems :: ContextValue String -> TemplateRunner String (Context String, [Item String])
getAsItems = fromValue

getAsStrings :: ContextValue String -> TemplateRunner String (Context String, [Item String])
getAsStrings x = do
  bodies <- fromValue x :: TemplateRunner String [String]
  items <- forM bodies \body -> itemSetBody body <$> tplItem
  return (bodyField "item", items)

forEachField :: Context String
forEachField = functionField3 "forEach" f
  where
    f (forEachKey :: ContextValue String) (forEachItems :: ContextValue String) (blocks :: [Block]) = do
      keyId <- getKey forEachKey
      keyItemPairs <- fromValue forEachItems :: TemplateRunner String [(ContextValue String, ContextValue String)]
      keyItemPairs `forM` \(key, items) ->
        tplWithContext (constField keyId key) do
          applyForLoop items blocks
    getKey block = case block of
      UndefinedValue k _ _ _ -> return k -- allow identifier as key
      StringValue k -> return k
      _ -> tplFail "forEach: key must be a string or identifier"

defaultField :: forall a. Context a
defaultField = functionField2 "default" f
  where
    f (default' :: ContextValue a) (arg :: ContextValue a) =
      isTruthy arg <&> \case
        True -> arg
        False -> default'

linkedTitleField :: Context String
linkedTitleField = constField "linkedTitle" f
  where
    f :: FunctionValue String String String
    f filePath = do
      tplWithItem (Item (fromFilePath filePath) "") do
        makeLink <$> getField "title" <*> getField "url"
      where
        getField key = do
          context <- tplContext
          fromValue =<< unContext context key
        makeLink title url
          | ".html" `isSuffixOf` filePath = "<a href=\"" ++ url ++ "\" title=\"" ++ escapeHtml title ++ "\">" ++ escapeHtml title ++ "</a>"
          | ".md" `isSuffixOf` filePath = "[" ++ escapeHtml title ++ "](" ++ url ++ " \"" ++ escapeHtml title ++ "\")"
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

bodyField :: String -> Context String
bodyField key = field key $ return . itemBody

urlField :: String -> Context a
urlField key = field key $ getUri key . itemIdentifier

getUri :: String -> Identifier -> TemplateRunner a String
getUri key id' = lift do
  maybeRoute <- getRoute id'
  definitelyRoute <-
    maybe
      (fail $ "no route by " ++ show key ++ " found for item " ++ show id')
      (return . ("/" ++))
      maybeRoute
  let uri = stripSuffix "index.html" definitelyRoute
  return if null uri then "/" else uri

absUrlField :: String -> String -> String -> Context a
absUrlField key siteUrlKey urlKey = field key f
  where
    f _ = do
      context <- tplContext
      siteUrl <- fromValue =<< unContext context siteUrlKey
      url <- fromValue =<< unContext context urlKey
      return (siteUrl ++ url :: String)

getUrlField :: String -> Context a
getUrlField key = functionField key f
  where
    f (filePath :: FilePath) = do
      let id' = fromFilePath filePath
      getUri key id'

getAbsUrlField :: forall a. String -> String -> String -> Context a
getAbsUrlField key siteUrlKey urlForKey = functionField key f
  where
    f (filePath :: FilePath) = do
      context <- tplContext
      siteUrl <- fromValue =<< unContext context siteUrlKey
      urlFor <- fromValue =<< unContext context urlForKey
      url <- urlFor (intoValue filePath :: ContextValue a)
      return (siteUrl ++ url :: String)

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
    f item =
      lift $
        (foldl (<|>))
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

putBlockField :: String -> Context a
putBlockField key = functionField2 key f
  where
    f (name :: String) (blocks :: [Block]) = do
      tplPut $ constField name blocks

addBlockField :: String -> Context a
addBlockField key = functionField2 key f
  where
    f (name :: String) (blocks :: [Block]) = do
      current <- tplGet name `catchError` \_ -> return []
      tplPut $ constField name (current ++ blocks)
