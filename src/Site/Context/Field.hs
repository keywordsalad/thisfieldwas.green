module Site.Context.Field where

import Data.String.Utils (endswith)
import Site.Common
import Site.Util

getCodeField :: Context String
getCodeField = functionField "getCode" f
  where
    f [lexer, contentsPath] _ = fmap wrapCode body
      where
        wrapCode code = "``` " ++ lexer ++ "\n" ++ code ++ "\n```"
        body = loadSnapshotBody item "code"
        item = fromFilePath $ "code/" ++ contentsPath
    f args item = error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = "expected [lexer, contentsPath] but received " ++ show args

imgField :: Context String
imgField = functionField "img" f
  where
    f [imgId, src] = f [imgId, src, ""]
    f [imgId, src, title] = f [imgId, src, title, ""]
    f [imgId, src, title, alt] =
      loadAndApplyTemplate
        "partials/image.html"
        ( constField "imgId" imgId
            <> constField "imgSrc" src
            <> constField "imgTitle" title
            <> constField "imgAlt" alt
        )
        >=> relativizeUrls
        >=> return . itemBody
    f args = \item -> error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = "expected [imgId, imgSrc, imgTitle, imgAlt] but received " ++ show args

youtubeField :: Context String
youtubeField = functionField "youtube" f
  where
    f [asideId, videoId] = f [asideId, videoId, ""]
    f [asideId, videoId, title] =
      loadAndApplyTemplate
        "partials/youtube.html"
        ( constField "youtubeAsideId" asideId
            <> constField "youtubeVideoId" videoId
            <> constField "youtubeVideoTitle" title
        )
        >=> relativizeUrls
        >=> return . itemBody
    f args = \item -> error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = "expected [youtubeAsideId, youtubeVideoId, youtubeVideoTitle] but received " ++ show args

getRouteField :: Context String
getRouteField = functionField "getRoute" f
  where
    f [filePath] _ = getUrlFromRoute (fromFilePath filePath)
    f args item = error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = "expected [filePath] but received " ++ show args

getUrlFromRoute :: Identifier -> Compiler String
getUrlFromRoute id' =
  getRoute id' >>= \case
    Just r -> return $ "/" ++ stripIndex r
    Nothing -> error $ "no route to " ++ show id'

commentField :: Context String
commentField = functionField "comment" \_ _ -> return ""

linkedTitleField :: Context String
linkedTitleField = functionField fieldKey f
  where
    fieldKey = "linkedTitle"
    f [filePath] _ = do
      let id' = fromFilePath filePath
      url <- getUrlFromRoute id'
      item :: Item String <- load id'
      let ctx = metadataField <> titleField "title" <> constField "title" url
      title <- unContext ctx "title" [] item >>= \case
        StringField s -> return s
        _ -> error $ "Could not resolve title in " ++ show id'
      return $ makeLink title url
      where
        isHtml = endswith ".html" filePath
        isMarkdown = endswith ".md" filePath || endswith ".markdown" filePath
        makeLink title url
          | isHtml = "<a href=\"" ++ url ++ "\">" ++ escapeHtml title ++ "</a>"
          | isMarkdown = "[" ++ escapeHtml title ++ "](" ++ url ++ ")"
          | otherwise = title ++ " <" ++ url ++ ">"
    f args item = error $ msg ++ " in " ++ show (itemIdentifier item)
      where
        msg = "expected [filePath] but received " ++ show args
