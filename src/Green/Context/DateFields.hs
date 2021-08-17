module Green.Context.DateFields (dateFields) where

import Data.List (tails)
import Data.String.Utils
import Green.Common
import Green.Config
import Green.Template.Context
import Green.Util
import qualified Hakyll as H

dateFields :: SiteConfig -> Context a
dateFields config =
  mconcat fields
    <> longDateFormatField
    <> shortDateFormatField
    <> timeFormatField
  where
    fields = uncurry mkFields <$> fieldKeys
    mkFields f k = f timeLocale k
    fieldKeys =
      [ (dateField, "date"),
        (publishedField, "published"),
        (updatedField, "updated")
      ]
    timeLocale = config ^. siteTimeLocale
    longDateFormatField = constField "longDate" $ displayFormat ^. displayDateLongFormat
    shortDateFormatField = constField "shortDate" $ displayFormat ^. displayDateShortFormat
    timeFormatField = constField "timeOnly" $ displayFormat ^. displayTimeFormat
    displayFormat = config ^. siteDisplayFormat

dateField :: TimeLocale -> String -> Context a
dateField = mconcat . (fns <*>) . pure
  where
    fns = [dateField' ["published", "date"], dateFromFilePathField]

publishedField :: TimeLocale -> String -> Context a
publishedField = dateField' ["published"]

updatedField :: TimeLocale -> String -> Context a
updatedField = dateField' ["updated"]

dateField' :: forall a. [String] -> TimeLocale -> String -> Context a
dateField' sourceKeys timeLocale targetKey = constField targetKey f
  where
    f :: FunctionValue String String a
    f dateFormat _ item = do
      maybeDate <- dateFromMetadata sourceKeys timeLocale item
      let maybeFormatted = formatTime timeLocale dateFormat <$> maybeDate
      maybe notFound return maybeFormatted
    notFound = noResult $ "Could not find date field field " ++ show targetKey ++ " from metadata keys " ++ show sourceKeys

dateFromMetadata :: [String] -> TimeLocale -> Item a -> Compiler (Maybe LocalTime)
dateFromMetadata sourceKeys timeLocale item = do
  maybeDates <- mapM findDate sourceKeys
  return $ firstMaybe maybeDates
  where
    id' = itemIdentifier item
    tryParseDate' = tryParseDate timeLocale metadataDateFormats
    findDate sourceKey = do
      maybeString <- H.lookupString sourceKey <$> H.getMetadata id'
      return (tryParseDate' =<< maybeString)

dateFromFilePathField :: forall a. TimeLocale -> String -> Context a
dateFromFilePathField timeLocale targetKey = constField targetKey f
  where
    f :: FunctionValue String String a
    f dateFormat _ item = maybe notFound return maybeFormatted
      where
        maybeDate = resolveDateFromFilePath timeLocale item
        maybeFormatted = formatTime timeLocale dateFormat <$> maybeDate
        notFound = noResult $ "Could not find " ++ show targetKey ++ " from file path " ++ show filePath
        filePath = toFilePath (itemIdentifier item)

resolveDateFromFilePath :: TimeLocale -> Item a -> Maybe LocalTime
resolveDateFromFilePath timeLocale item =
  let paths = splitDirectories $ dropExtension $ toFilePath $ itemIdentifier item
   in firstMaybe $
        dateFromPath
          <$> [take 3 $ split "-" fnCand | fnCand <- reverse paths]
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
