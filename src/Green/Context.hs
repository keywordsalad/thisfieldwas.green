module Green.Context
  ( module Green.Context,
    module Green.Context.GitCommits,
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Data.List (intercalate, tails)
import Data.String.Utils
import Data.Time (ZonedTime)
import Data.Time.Format
import Green.Config
import Green.Context.GitCommits
import Green.Util (dropIndex, stripSuffix)
import Hakyll hiding (dateField)
import Lens.Micro
import System.FilePath

baseContext :: SiteConfig -> Context String
baseContext config = do
  let context =
        mconcat
          [ siteRootField (config ^. siteRoot),
            linkedInProfileField (config ^. siteLinkedInProfile),
            trimmedUrlField,
            gitCommits (config ^. siteGitWebUrl),
            imgField,
            youtubeField,
            getRouteField,
            commentField,
            defaultContext,
            bodyClassField "default",
            contactEmailField (config ^. siteAuthorEmail),
            dateFields (config ^. siteTimeLocale)
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
dateFields timeLocale =
  mconcat $ formatField <$> keys
  where
    formatField (skey, tkey) = dateField timeLocale skey tkey targetFormat
    targetFormat = "%B %e, %Y"
    keys =
      [ ("date", "updated"),
        ("date", "published"),
        ("date", "date"),
        ("published", "published"),
        ("published", "date"),
        ("updated", "updated"),
        ("updated", "published"),
        ("updated", "date")
      ]

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

dateField :: TimeLocale -> String -> Context String
dateField timeLocale =
  mconcat $
    [fromMetadata', dateFieldFromPath'] <*> pure timeLocale
  where
    fromMetadata' =
      mconcat $
        dateFromMetadataField timeLocale "date"
          <$> ["date", "published", "updated"]

dateFromMetadataField :: TimeLocale -> String -> String -> String -> Context String
dateFromMetadataField timeLocale targetKey sourceKey targetFormat = field targetKey \i -> do
  maybeMetadata <- lookupString sourceKey <$> getMetadata (itemIdentifier i)
  let maybeDate :: Maybe ZonedTime = tryParse =<< maybeMetadata
  let directories = splitDirectories $ (dropExtension . toFilePath) (itemIdentifier i)
  return $ maybe "" format maybeDate
  where
    format = formatTime timeLocale targetFormat
    parse = parseTimeM True timeLocale
    tryParse = foldl (<|>) Nothing . flip fmap sourceFormats . flip parse
    directories = splitDirectories . (dropExtension . toFilePath) . itemIdentifier
    dateParts paths =
      let dashedDates =
            parseTimeM True timeLocale "%Y-%m-%d" . intercalate "-" <$>
              [take 3 $ splitAll "-" part | part <- reverse paths]
                ++ [part | part <- map (take 3) . reverse . tails $ paths]
    sourceFormats =
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

dateFromPathField' :: TimeLocale -> String -> Context String
dateFromPathField' timeLocale key = Context \k _ i -> do
  if k == key
    then f
    else return EmptyField
  where
    f = do
      let paths = splitDirectories $ (dropExtension . toFilePath) (itemIdentifier i)
      return $ foldl (<|>) Nothing (tryParse <$> dateParts paths)
      where
        dateParts paths =
          [take 3 $ splitAll "-" fnCand | fnCand <- reverse paths]
            ++ [fnCand | fnCand <- map (take 3) . reverse . tails $ paths]
        tryParse = parseTimeM True timeLocale "%Y-%m-%d" . intercalate "-"
