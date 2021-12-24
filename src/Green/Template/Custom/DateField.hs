module Green.Template.Custom.DateField (dateFields) where

import Data.List (tails)
import Data.String.Utils
import Green.Common
import Green.Config
import Green.Template.Context
import Green.Util

dateFields :: SiteConfig -> Context a
dateFields config =
  mconcat
    [ dateField "date" timeLocale,
      publishedField "published" timeLocale,
      updatedField "updated" timeLocale,
      constField "longDate" (displayFormat ^. displayDateLongFormat),
      constField "shortDate" (displayFormat ^. displayDateShortFormat),
      constField "timeOnly" (displayFormat ^. displayTimeFormat),
      dateFormatField "dateAs" timeLocale
    ]
  where
    timeLocale = config ^. siteTimeLocale
    displayFormat = config ^. siteDisplayFormat

dateFormatField :: String -> TimeLocale -> Context a
dateFormatField key timeLocale = functionField2 key f
  where
    f (dateFormat :: String) (dateString :: String) = do
      date <- deserializeTime dateString
      return $ formatTime timeLocale dateFormat date
    deserializeTime = parseTimeM' timeLocale normalizedFormat

dateField :: String -> TimeLocale -> Context a
dateField key timeLocale = field key f
  where
    f item =
      lift $
        dateFromMetadata timeLocale ["date", "published"] item
          <|> dateFromFilePath timeLocale item

publishedField :: String -> TimeLocale -> Context a
publishedField key timeLocale = field key f
  where
    f = lift . dateFromMetadata timeLocale ["published"]

updatedField :: String -> TimeLocale -> Context a
updatedField key timeLocale = field key f
  where
    f = lift . dateFromMetadata timeLocale ["updated"]

dateFromMetadata :: TimeLocale -> [String] -> Item a -> Compiler String
dateFromMetadata timeLocale sourceKeys item =
  firstAlt $ findDate <$> sourceKeys
  where
    id' = itemIdentifier item
    findDate sourceKey = do
      metadata <- getMetadata id'
      let maybeDate = lookupString sourceKey metadata
      debugCompiler $ "Source key " ++ show sourceKey ++ " returned " ++ show maybeDate ++ " for date from metadata"
      let notFound = noResult $ "tried date from metadata key " ++ show sourceKey
      let maybeFrozenDate = serializeTime =<< maybeDate
      maybe notFound return maybeFrozenDate
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
normalizedFormat = "%Y-%m-%dT%H:%M:%S%EZ"

metadataDateFormats :: [String]
metadataDateFormats =
  [ "%Y-%m-%d",
    normalizedFormat,
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S%EZ",
    "%Y-%m-%d %H:%M:%S",
    "%a, %d %b %Y %H:%M:%S %EZ",
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
