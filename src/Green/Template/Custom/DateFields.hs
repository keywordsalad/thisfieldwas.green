module Green.Template.Custom.DateFields (dateFields) where

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
    f (dateFormat :: String) (dateString :: String) _ _ = do
      date <- thawTime timeLocale dateString
      return $ formatTime timeLocale dateFormat date

dateField :: String -> TimeLocale -> Context a
dateField key timeLocale = field key f
  where
    f item =
      dateFromMetadata timeLocale ["date", "published"] item
        <|> dateFromFilePath timeLocale item

publishedField :: String -> TimeLocale -> Context a
publishedField key timeLocale = field key f
  where
    f = dateFromMetadata timeLocale ["published"]

updatedField :: String -> TimeLocale -> Context a
updatedField key timeLocale = field key f
  where
    f = dateFromMetadata timeLocale ["updated"]

dateFromMetadata :: TimeLocale -> [String] -> Item a -> Compiler String
dateFromMetadata timeLocale sourceKeys item =
  cached cacheKey do
    firstAlt $ findDate <$> sourceKeys
  where
    cacheKey = "Green.Template.Custom.DateFields.dateFromMetadata:" ++ show sourceKeys
    id' = itemIdentifier item
    findDate sourceKey = do
      metadata <- getMetadata id' :: Compiler Metadata
      let maybeDate = lookupString sourceKey metadata
      debugCompiler $ "Source key " ++ show sourceKey ++ " returned " ++ show maybeDate ++ " for date from metadata"
      let notFound = noResult $ "Could not find metadata date using key " ++ show sourceKey
      let maybeFrozenDate = freezeTime timeLocale metadataDateFormats =<< maybeDate
      maybe notFound return maybeFrozenDate

dateFromFilePath :: TimeLocale -> Item a -> Compiler String
dateFromFilePath timeLocale item =
  cached cacheKey do
    dateFromPath
      <|> noResult ("Could not find file path date from " ++ show (toFilePath $ itemIdentifier item))
  where
    cacheKey = "Green.Template.Custom.DateFields.dateFromFilePath"
    dateFromPath =
      firstAlt $
        dateFromPath' . intercalate "-"
          <$> ( [take 3 $ split "-" fnCand | fnCand <- reverse paths]
                  ++ (fmap (take 3) <$> reverse (tails paths))
              )
    paths = splitDirectories $ dropExtension $ toFilePath $ itemIdentifier item
    dateFromPath' path = do
      debugCompiler $ "Trying to parse date from path " ++ show path
      date <- parseTimeM True timeLocale "%Y-%m-%d" path :: Compiler ZonedTime
      return $ formatTime timeLocale normalizedFormat date

freezeTime :: TimeLocale -> [String] -> String -> Maybe String
freezeTime timeLocale dateFormats dateString = do
  date <- firstAlt (parse <$> dateFormats) :: Maybe ZonedTime
  return $ formatTime timeLocale normalizedFormat date
  where
    parse dateFormat = parseTimeM True timeLocale dateFormat dateString

thawTime :: TimeLocale -> String -> Compiler ZonedTime
thawTime timeLocale = parseTimeM True timeLocale normalizedFormat

normalizedFormat :: String
normalizedFormat = "%Y-%m-%dT%H:%M:%S%EZ"

metadataDateFormats :: [String]
metadataDateFormats =
  [ "%Y-%m-%d",
    "%Y-%m-%dT%H:%M:%S%EZ",
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
