{-# LANGUAGE AllowAmbiguousTypes #-}

module Green.Template.Fields where

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
    f (filePath :: String) context item =
      let id' = fromFilePath (basePath </> filePath <.> "html")
       in itemValue context <$> loadAndApplyTemplate id' context item

layoutField :: String -> FilePath -> Context String
layoutField key basePath = functionField2 key f
  where
    f (filePath :: FilePath) (blocks :: [Block]) context item = do
      let layoutId = fromFilePath $ basePath </> filePath <.> "html"
      reduced <- reduceBlocks context blocks item
      template <- loadTemplateBody layoutId
      itemValue context <$> applyTemplate template context (itemSetBody reduced item)

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
            ItemsValue ctx xs -> Just . mconcat <$> mapM (reduce ctx) xs
            ContextValue ctx -> Just <$> reduce ctx item
            x -> fail $ "Unexpected " ++ show x ++ " in {{#for}}"
        --
        False -> return Nothing
      where
        reduce ctx = reduceBlocks (ctx <> context) blocks

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
      let id' = fromFilePath filePath
      getRoute id' >>= \case
        Just r -> return $ "/" ++ stripSuffix "index.html" r
        Nothing -> error $ "no route to " ++ show id'

linkedTitleField :: Context String
linkedTitleField = constField "linkedTitle" f
  where
    f :: FunctionValue String String String
    f filePath context item = do
      linkedItem <- load (fromFilePath filePath)
      makeLink <$> getField "title" linkedItem <*> getField "url" linkedItem
      where
        getField key linkedItem = tryWithError key item $ fromValue =<< unContext context key linkedItem
        makeLink title url
          | endswith ".html" filePath = "<a href=\"" ++ url ++ "\">" ++ escapeHtml title ++ "</a>"
          | endswith ".md" filePath = "[" ++ escapeHtml title ++ "](" ++ url ++ ")"
          | otherwise = title ++ " <" ++ url ++ ">"

metadataField :: forall a. Context a
metadataField = Context f
  where
    f :: ContextFunction a
    f key item = do
      m <- getMetadata (itemIdentifier item)
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
       in maybe empty' toUrl <$> getRoute id'

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
      takeTeaser "" <$> loadSnapshotBody (itemIdentifier item) snapshot

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
