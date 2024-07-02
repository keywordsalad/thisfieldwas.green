module Green.Hakyllbars.Field.Date
  ( DateConfig (..),
    defaultDateConfigWith,
    dateFields,
    dateFormatField,
    dateField,
    publishedField,
    updatedField,
    getLastModifiedDate,
    isPublishedField,
    isUpdatedField,
    dateFromMetadata,
    normalizedDateTimeFormat,
    parseTimeM',
  )
where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (tails)
import Data.String.Utils
import Green.Hakyllbars.Common
import Green.Hakyllbars.Context
import Green.Hakyllbars.Util

data DateConfig = DateConfig
  { -- | The locale to use for date formatting.
    dateConfigLocale :: TimeLocale,
    -- | The current time (or time at which the site generator is running).
    dateConfigCurrentTime :: ZonedTime,
    -- | The format to use for long dates (i.e. date with time).
    dateConfigDateLongFormat :: String,
    -- | The format to use for short dates (i.e. date without time).
    dateConfigDateShortFormat :: String,
    -- | The format to use for time only.
    dateConfigTimeFormat :: String,
    -- | The format to use for machine-readable dates.
    dateConfigRobotDateFormat :: String,
    -- | The format to use for machine-readable times.
    dateConfigRobotTimeFormat :: String
  }

-- | Creates a default date configuration with the given locale and current time.
defaultDateConfigWith :: TimeLocale -> ZonedTime -> DateConfig
defaultDateConfigWith locale currentTime =
  DateConfig
    { dateConfigLocale = locale,
      dateConfigCurrentTime = currentTime,
      dateConfigDateLongFormat = "%B %e, %Y %l:%M %P %EZ",
      dateConfigDateShortFormat = "%B %e, %Y",
      dateConfigTimeFormat = "%l:%M %p %EZ",
      dateConfigRobotDateFormat = "%Y-%m-%d",
      dateConfigRobotTimeFormat = "%Y-%m-%dT%H:%M:%S%Ez"
    }

-- | Creates a default date fields configuration with the given date config.
dateFields :: DateConfig -> Context a
dateFields config =
  mconcat
    [ dateField "date" (dateConfigLocale config) (dateConfigCurrentTime config),
      publishedField "published" (dateConfigLocale config),
      updatedField "updated" (dateConfigLocale config),
      isPublishedField "isPublished",
      isUpdatedField "isUpdated",
      constField "longDate" (dateConfigDateLongFormat config),
      constField "shortDate" (dateConfigDateShortFormat config),
      constField "timeOnly" (dateConfigTimeFormat config),
      constField "robotDate" (dateConfigRobotDateFormat config),
      constField "robotTime" (dateConfigRobotTimeFormat config),
      constField "rfc822" rfc822DateFormat,
      dateFormatField "dateAs" (dateConfigLocale config)
    ]

-- | Gets a date formatted with the given format.
dateFormatField :: String -> TimeLocale -> Context a
dateFormatField key timeLocale = functionField2 key f
  where
    f (dateFormat :: String) (dateString :: String) = do
      date <- deserializeTime dateString
      return $ formatTime timeLocale dateFormat date
    deserializeTime = parseTimeM' timeLocale normalizedDateTimeFormat

-- | Gets the date relative to the configured time locale and current time from the "date" or "published" fields.
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

-- | Gets the published date of an item from the metadata fields "published" or "date".
publishedField :: String -> TimeLocale -> Context a
publishedField key timeLocale = field key f
  where
    f =
      lift
        . getMetadata
        . itemIdentifier
        >=> tplWithCall key
        . lift
        . maybe (noResult $ "Tried published field " ++ show key) return
        . dateFromMetadata timeLocale ["published", "date"]

-- | Gets the updated date of an item from the metadata fields "updated", "published", or "date".
updatedField :: String -> TimeLocale -> Context a
updatedField key timeLocale = field key f
  where
    f =
      lift
        . getMetadata
        . itemIdentifier
        >=> tplWithCall key
        . lift
        . maybe (noResult $ "Tried updated field " ++ show key) return
        . dateFromMetadata timeLocale ["updated", "published", "date"]

-- | Gets the last modified date of an item from the metadata fields "updated", "published", or "date", or the file path
-- if it contains a date.
getLastModifiedDate :: TimeLocale -> Item a -> Compiler ZonedTime
getLastModifiedDate timeLocale item = do
  metadata <- getMetadata $ itemIdentifier item
  let maybeDateString = dateFromMetadata timeLocale ["updated", "published", "date"] metadata
  dateString <- maybe (dateFromFilePath timeLocale item) return maybeDateString
  parseTimeM' timeLocale "%Y-%m-%dT%H:%M:%S%Ez" dateString

-- | Gets a date from the given metadata fields.
dateFromMetadata ::
  -- | The time locale to use.
  TimeLocale ->
  -- | The list of metadata keys to search for.
  [String] ->
  Metadata ->
  Maybe String
dateFromMetadata timeLocale sourceKeys metadata =
  firstAlt $ findDate <$> sourceKeys
  where
    findDate sourceKey =
      serializeTime =<< lookupString sourceKey metadata
    serializeTime dateString = do
      date <- firstAlt (parse dateString <$> metadataDateFormats)
      return $ normalizedTime timeLocale date
    parse = flip $ parseTimeM True timeLocale

-- | Gets a date from the item's file path.
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
normalizedTime = flip formatTime normalizedDateTimeFormat

normalizedDateTimeFormat :: String
normalizedDateTimeFormat = "%Y-%m-%dT%H:%M:%S%Ez"

rfc822DateFormat :: String
rfc822DateFormat = "%a, %d %b %Y %H:%M:%S %Z"

-- | Supported date formats to read from metadata.
metadataDateFormats :: [String]
metadataDateFormats =
  [ "%Y-%m-%d",
    normalizedDateTimeFormat,
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

-- | Gets whether the item is published.
isPublishedField :: String -> Context a
isPublishedField key = field key f
  where
    f item = lift do
      getMetadata (itemIdentifier item)
        <&> isJust
        . KeyMap.lookup (Key.fromString "published")

-- | Gets whether the item has been updated.
isUpdatedField :: String -> Context a
isUpdatedField key = field key f
  where
    f item = lift do
      getMetadata (itemIdentifier item)
        <&> isJust
        . KeyMap.lookup (Key.fromString "updated")
