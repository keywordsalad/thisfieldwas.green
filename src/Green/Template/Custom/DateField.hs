module Green.Template.Custom.DateField where

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (tails)
import Data.String.Utils
import Green.Common
import Green.Config
import Green.Template.Context
import Green.Util

dateFields :: SiteConfig -> Context a
dateFields config =
  mconcat
    [ dateField "date" timeLocale currentTime,
      publishedField "published" timeLocale,
      updatedField "updated" timeLocale,
      isPublishedField "isPublished",
      isUpdatedField "isUpdated",
      constField "longDate" (displayFormat ^. displayDateLongFormat),
      constField "shortDate" (displayFormat ^. displayDateShortFormat),
      constField "timeOnly" (displayFormat ^. displayTimeFormat),
      constField "robotTime" (displayFormat ^. displayRobotTime),
      constField "rfc822" rfc822DateFormat,
      dateFormatField "dateAs" timeLocale
    ]
  where
    timeLocale = config ^. siteTimeLocale
    currentTime = config ^. siteCurrentTime
    displayFormat = config ^. siteDisplayFormat

dateFormatField :: String -> TimeLocale -> Context a
dateFormatField key timeLocale = functionField2 key f
  where
    f (dateFormat :: String) (dateString :: String) = do
      date <- deserializeTime dateString
      return $ formatTime timeLocale dateFormat date
    deserializeTime = parseTimeM' timeLocale normalizedFormat

dateField :: String -> TimeLocale -> ZonedTime -> Context a
dateField key timeLocale currentTime = field key f
  where
    f item = do
      metadata <- lift . getMetadata $ itemIdentifier item
      tplWithCall key . lift $
        do
          let maybeDateString = dateFromMetadata timeLocale ["date", "published"] metadata
          maybe (dateFromFilePath timeLocale item) return maybeDateString
            <|> return (formatTime timeLocale "%Y-%m-%dT%H:%M:%S%Ez" currentTime)

publishedField :: String -> TimeLocale -> Context a
publishedField key timeLocale = field key f
  where
    f =
      lift . getMetadata . itemIdentifier
        >=> tplWithCall key . lift
          . maybe (noResult $ "Tried published field " ++ show key) return
          . dateFromMetadata timeLocale ["published", "date"]

updatedField :: String -> TimeLocale -> Context a
updatedField key timeLocale = field key f
  where
    f =
      lift . getMetadata . itemIdentifier
        >=> tplWithCall key . lift
          . maybe (noResult $ "Tried updated field " ++ show key) return
          . dateFromMetadata timeLocale ["updated", "published", "date"]

getLastModifiedDate :: TimeLocale -> Item a -> Compiler ZonedTime
getLastModifiedDate timeLocale item = do
  metadata <- getMetadata $ itemIdentifier item
  let maybeDateString = dateFromMetadata timeLocale ["updated", "published", "date"] metadata
  dateString <- maybe (dateFromFilePath timeLocale item) return maybeDateString
  parseTimeM' timeLocale "%Y-%m-%dT%H:%M:%S%Ez" dateString

dateFromMetadata :: TimeLocale -> [String] -> Metadata -> Maybe String
dateFromMetadata timeLocale sourceKeys metadata =
  firstAlt $ findDate <$> sourceKeys
  where
    findDate sourceKey =
      serializeTime =<< lookupString sourceKey metadata
    serializeTime dateString = do
      date <- firstAlt (parse dateString <$> metadataDateFormats)
      return $ normalizedTime timeLocale date
    parse = flip $ parseTimeM True timeLocale

dateFromFilePath :: TimeLocale -> Item a -> Compiler String
dateFromFilePath timeLocale item =
  dateFromPath
    <|> noResult ("Could not find file path date from " ++ show (toFilePath $ itemIdentifier item))
  where
    dateFromPath =
      firstAlt $
        dateFromPath' . intercalate "-"
          <$> ( [take 3 $ split "-" fnCand | fnCand <- reverse paths]
                  ++ (fmap (take 3) <$> reverse (tails paths))
              )
    paths = splitDirectories $ dropExtension $ toFilePath $ itemIdentifier item
    dateFromPath' path = do
      debugCompiler $ "Trying to parse date from path " ++ show path
      date <- parseTimeM' timeLocale "%Y-%m-%d" path
      return $ normalizedTime timeLocale date

parseTimeM' :: (MonadFail m) => TimeLocale -> String -> String -> m ZonedTime
parseTimeM' = parseTimeM True

normalizedTime :: TimeLocale -> ZonedTime -> String
normalizedTime = flip formatTime normalizedFormat

normalizedFormat :: String
normalizedFormat = "%Y-%m-%dT%H:%M:%S%Ez"

rfc822DateFormat :: String
rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %Z"

metadataDateFormats :: [String]
metadataDateFormats =
  [ "%Y-%m-%d",
    normalizedFormat,
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S %EZ",
    "%Y-%m-%d %H:%M:%S%Ez",
    "%Y-%m-%d %H:%M:%S",
    rfc822DateFormat,
    "%a, %d %b %Y %H:%M:%S",
    "%B %e, %Y %l:%M %p %EZ",
    "%B %e, %Y %l:%M %p",
    "%b %e, %Y %l:%M %p %EZ",
    "%b %e, %Y %l:%M %p",
    "%B %e, %Y",
    "%B %d, %Y",
    "%b %e, %Y",
    "%b %d, %Y"
  ]

isPublishedField :: String -> Context a
isPublishedField key = field key f
  where
    f item = lift do
      getMetadata (itemIdentifier item)
        <&> isJust . KeyMap.lookup (Key.fromString "published")

isUpdatedField :: String -> Context a
isUpdatedField key = field key f
  where
    f item = lift do
      getMetadata (itemIdentifier item)
        <&> isJust . KeyMap.lookup (Key.fromString "updated")
