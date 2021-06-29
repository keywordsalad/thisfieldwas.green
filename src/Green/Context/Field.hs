module Green.Context.Field where

import Control.Monad ((>=>))
import Data.Maybe (fromJust)
import Data.String.Utils
import Data.Time (ZonedTime)
import Data.Time.Format
import Green.Util
import Hakyll hiding (dateField)
import Lens.Micro

trimIndexUrlField :: String -> Context a
trimIndexUrlField = mapContext dropIndex . urlField

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
      loadAndApplyTemplate
        "_templates/image.html"
        ( constField "imgId" imgId
            <> constField "imgSrc" src
            <> constField "imgTitle" title
            <> constField "imgAlt" alt
        )
        >=> relativizeUrls
        >=> return . itemBody
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
      loadAndApplyTemplate
        "_templates/youtube.html"
        ( constField "youtubeAsideId" asideId
            <> constField "youtubeVideoId" videoId
            <> constField "youtubeVideoTitle" title
        )
        >=> relativizeUrls
        >=> return . itemBody
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

dateField :: TimeLocale -> String -> String -> Context String
dateField timeLocale key format = field key \i -> do
  maybeField <- lookupString key <$> getMetadata (itemIdentifier i)
  maybeDate <- p <$> maybeField
  return $ maybe "" f maybeDate
  where
    f :: ZonedTime -> String
    f = formatTime timeLocale format
    p = fromJust . parseTime timeLocale timeFormat
