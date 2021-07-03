module Green.Context
  ( module Green.Context,
    module Green.Context.GitCommits,
  )
where

import Data.List (intercalate, tails)
import Data.String.Utils (endswith)
import Green.Common
import Green.Config
import Green.Context.GitCommits
import Green.Util (dropIndex, firstMaybe, stripSuffix)

baseContext :: SiteConfig -> Context String
baseContext config = do
  let context =
        mconcat
          [ siteRootField (config ^. siteRoot),
            linkedInProfileField (config ^. siteLinkedInProfile),
            trimmedUrlField,
            dateFields (config ^. siteTimeLocale),
            gitCommits (config ^. siteGitWebUrl),
            imgField,
            youtubeField,
            getRouteField,
            commentField,
            defaultContext,
            bodyClassField "default",
            contactEmailField (config ^. siteAuthorEmail)
          ]
      dependentContexts =
        [ getCodeField,
          linkedTitleField
        ]
   in mconcat (dependentContexts <*> pure context) <> context

bodyClassField :: String -> Context String
bodyClassField = constField "body-class"

contactEmailField :: String -> Context String
contactEmailField = constField "contactEmail"

linkedInProfileField :: String -> Context String
linkedInProfileField = constField "linkedInProfile"

dateFields :: TimeLocale -> Context String
dateFields = undefined

-- | Trims @index.html@ from @$url$@'s
trimmedUrlField :: Context String
trimmedUrlField = mapContext dropIndex (urlField "url")

siteRootField :: String -> Context String
siteRootField = constField "siteRoot"

getCodeField :: Context String -> Context String
getCodeField siteContext' = functionField key f
  where
    key = "getCode"
    f [lexer, contentsPath] _ =
      let localContext = constField "lexer" lexer <> siteContext'
       in loadSnapshot codeId "code"
            >>= loadAndApplyTemplate templateId localContext
            <&> itemBody
      where
        codeId = fromFilePath $ "code/" ++ contentsPath
        templateId = fromFilePath "_templates/code.md"
    f args item = error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = key ++ " expected [lexer, contentsPath] but received " ++ show args

imgField :: Context String
imgField = functionField key f
  where
    key = "img"
    f [imgId, src] = f [imgId, src, ""]
    f [imgId, src, title] = f [imgId, src, title, ""]
    f [imgId, src, title, alt] =
      return . itemBody
        <=< relativizeUrls
        <=< loadAndApplyTemplate
          "_templates/image.html"
          ( mconcat
              [ constField "imgId" imgId,
                constField "imgSrc" src,
                constField "imgTitle" title,
                constField "imgAlt" alt
              ]
          )
    f args = \item -> error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = key ++ " expected [imgId, imgSrc, imgTitle, imgAlt] but received " ++ show args

youtubeField :: Context String
youtubeField = functionField key f
  where
    key = "youtube"
    f [videoId] = f [videoId, ""]
    f [videoId, asideId] = f [videoId, asideId, ""]
    f [videoId, asideId, title] =
      return . itemBody
        <=< relativizeUrls
        <=< loadAndApplyTemplate
          "_templates/youtube.html"
          ( mconcat
              [ constField "youtubeAsideId" asideId,
                constField "youtubeVideoId" videoId,
                constField "youtubeVideoTitle" title
              ]
          )
    f args = \item -> error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = key ++ " expected [youtubeAsideId, youtubeVideoId, youtubeVideoTitle] but received " ++ show args

commentField :: Context String
commentField = functionField "comment" \_ _ -> return ""

getRouteField :: Context String
getRouteField = functionField key f
  where
    key = "getRoute"
    f [filePath] _ = do
      let id' = fromFilePath filePath
      getRoute id' >>= \case
        Just r -> return $ "/" ++ stripSuffix "index.html" r
        Nothing -> error $ "no route to " ++ show id'
    f args item = error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = key ++ " expected [filePath] but received " ++ show args

unContextString :: Context String -> String -> [String] -> Item String -> Compiler String
unContextString context key args item =
  unContext context key args item >>= \case
    StringField s -> return s
    _ -> error $ "Got a non-string value in field " ++ key

linkedTitleField :: Context String -> Context String
linkedTitleField context = functionField linkedTitleKey f
  where
    linkedTitleKey = "linkedTitle"
    f [filePath] _ = do
      linkedItem <- load (fromFilePath filePath)
      makeLink <$> getTitle linkedItem <*> getUrl linkedItem
      where
        makeLink title url
          | isHtml = "<a href=\"" ++ url ++ "\">" ++ escapeHtml title ++ "</a>"
          | isMarkdown = "[" ++ escapeHtml title ++ "](" ++ url ++ ")"
          | otherwise = title ++ " <" ++ url ++ ">"
        getTitle = getField "title"
        getUrl = getField "url"
        isHtml = endswith ".html" filePath
        isMarkdown = endswith ".md" filePath || endswith ".markdown" filePath
        getField key = unContextString context key []
    f args item = error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = linkedTitleKey ++ " expected [filePath] but received " ++ show args

dateFromMetadataFields :: TimeLocale -> String -> [String] -> String -> Context String
dateFromMetadataFields timeLocale targetKey sourceKeys targetFormat = Context \k _ i ->
  if k == targetKey
    then f $ itemIdentifier i
    else return EmptyField
  where
    f id' = foldl (<|>) (return EmptyField) (findDate id' <$> sourceKeys)
    findDate id' sourceKey = do
      maybeString <- lookupString sourceKey <$> getMetadata id'
      let maybeDate = tryParseDate' =<< maybeString
      let maybeFormat = formatTime timeLocale targetFormat <$> maybeDate
      return $ maybe EmptyField StringField maybeFormat
    tryParseDate' :: String -> Maybe ZonedTime
    tryParseDate' = tryParseDate timeLocale sourceDateFormats
    sourceDateFormats =
      [ "%FT%T%Z",
        "%Y-%m-%d",
        "%Y-%m-%dT%H:%M:%S%Z",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M:%S%Z",
        "%Y-%m-%d %H:%M:%S",
        "%a, %d %b %Y %H:%M:%S %Z",
        "%a, %d %b %Y %H:%M:%S %Z",
        "%a, %d %b %Y %H:%M:%S",
        "%a, %d %b %Y %I:%M:%S %P %Z",
        "%a, %d %b %Y %I:%M:%S %P %Z",
        "%a, %d %b %Y %I:%M:%S %P ",
        "%a, %d %b %Y %I:%M:%S %p %Z",
        "%a, %d %b %Y %I:%M:%S %p %Z",
        "%a, %d %b %Y %I:%M:%S %p",
        "%B %e, %Y %l:%M %p",
        "%B %e, %Y",
        "%b %d, %Y"
      ]

tryParseDate :: (ParseTime a) => TimeLocale -> [String] -> String -> Maybe a
tryParseDate timeLocale dateFormats = firstMaybe . flip fmap dateFormats . parse
  where
    parse = flip $ parseTimeM True timeLocale

dateFromFilePath :: TimeLocale -> String -> String -> Context String
dateFromFilePath timeLocale targetKey targetFormat = Context \k _ i ->
  if k == targetKey
    then return . maybe EmptyField StringField . f $ itemIdentifier i
    else return EmptyField
  where
    f :: Identifier -> Maybe String
    f = fmap (formatTime timeLocale targetFormat) . tryParseDate'
    paths = splitDirectories . dropExtension . toFilePath
    tryParseDate' :: Identifier -> Maybe ZonedTime
    tryParseDate' id' =
      let paths' = paths id'
       in firstMaybe $
            dateFromPath
              <$> [take 3 $ splitAll "-" fnCand | fnCand <- reverse paths']
              ++ [fnCand | fnCand <- map (take 3) $ reverse $ tails paths']
    dateFromPath = tryParseDate timeLocale ["%Y-%m-%d"] . intercalate "-"
