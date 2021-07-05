module Green.Context.DateFields where

import Data.List (intercalate, tails)
import Green.Common
import Green.Config
import Green.Context.FieldError
import Green.Util

dateFields :: SiteConfig -> Context String
dateFields config =
  mconcat fields
    <> shortDateFormatField
    <> timeFormatField
  where
    timeLocale = config ^. siteTimeLocale
    displayFormat = config ^. siteDisplayFormat
    longDateFormat = displayFormat ^. displayDateLongFormat
    shortDateFormatField = constField "shortDate" $ displayFormat ^. displayDateShortFormat
    timeFormatField = constField "timeFormat" $ displayFormat ^. displayTimeFormat
    fields = uncurry mkFields <$> fieldKeys
    mkFields f k = f longDateFormat timeLocale k
    fieldKeys =
      [ (dateField, "date"),
        (publishedField, "published"),
        (updatedField, "updated")
      ]

dateField :: String -> TimeLocale -> String -> Context String
dateField = mconcat . (fns <*>) . pure
  where
    fns = [dateField' ["published", "date"], dateFromFilePathField]

publishedField :: String -> TimeLocale -> String -> Context String
publishedField = dateField' ["published"]

updatedField :: String -> TimeLocale -> String -> Context String
updatedField = dateField' ["updated"]

dateField' :: [String] -> String -> TimeLocale -> String -> Context String
dateField' sourceKeys defaultFormat timeLocale targetKey = functionField targetKey f
  where
    notFound = noResult $ "Could not find $" ++ targetKey ++ "$ from metadata keys " ++ show sourceKeys
    f [] item = f [defaultFormat] item
    f [dateFormat] item = do
      maybeDate :: Maybe LocalTime <- dateFromMetadata sourceKeys timeLocale item
      let maybeFormatted = formatTime timeLocale dateFormat <$> maybeDate
      maybe notFound return maybeFormatted
    f args item = fieldError targetKey ["dateFormat"] args item

dateFromMetadata :: (ParseTime a) => [String] -> TimeLocale -> Item String -> Compiler (Maybe a)
dateFromMetadata sourceKeys timeLocale item = do
  maybeDates <- mapM findDate sourceKeys
  return $ firstMaybe maybeDates
  where
    id' = itemIdentifier item
    tryParseDate' = tryParseDate timeLocale metadataDateFormats
    findDate sourceKey = do
      maybeString <- lookupString sourceKey <$> getMetadata id'
      return (tryParseDate' =<< maybeString)

dateFromFilePathField :: String -> TimeLocale -> String -> Context String
dateFromFilePathField defaultFormat timeLocale targetKey = functionField targetKey f
  where
    f [] item = f [defaultFormat] item
    f [dateFormat] item =
      let maybeFormatted = formatTime timeLocale dateFormat <$> maybeDate
       in maybe notFound return maybeFormatted
      where
        maybeDate :: Maybe LocalTime = resolveDateFromFilePath timeLocale item
        notFound = noResult $ "Could not find $" ++ targetKey ++ "$ from file path " ++ filePath
        filePath = toFilePath (itemIdentifier item)
    f args item = fieldError targetKey ["defaultFormat"] args item

resolveDateFromFilePath :: (ParseTime a) => TimeLocale -> Item String -> Maybe a
resolveDateFromFilePath timeLocale item =
  let paths = splitDirectories $ dropExtension $ toFilePath $ itemIdentifier item
   in firstMaybe $
        dateFromPath
          <$> [take 3 $ splitAll "-" fnCand | fnCand <- reverse paths]
          ++ [fnCand | fnCand <- map (take 3) $ reverse $ tails paths]
  where
    dateFromPath = tryParseDate timeLocale ["%Y-%m-%d"] . intercalate "-"

tryParseDate :: (ParseTime a) => TimeLocale -> [String] -> String -> Maybe a
tryParseDate timeLocale dateFormats = firstMaybe . flip fmap dateFormats . parse
  where
    parse = flip $ parseTimeM True timeLocale

metadataDateFormats :: [String]
metadataDateFormats =
  [ "%Y-%m-%d",
    "%Y-%m-%dT%H:%M:%S%Z",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S%Z",
    "%Y-%m-%d %H:%M:%S",
    "%a, %d %b %Y %H:%M:%S %Z",
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
