module Site.Context.Field where

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
    f (filePath : []) item = do
      getRoute id' >>= \case
        Just r -> return $ ("/" ++ stripIndex r)
        Nothing -> error $ "routeField in " ++ show fromId ++ ": no route to " ++ show id'
      where
        id' = fromFilePath filePath
        fromId = itemIdentifier item
    f _ item = error $ "routeField needs a filePath " ++ show (itemIdentifier item)

commentField :: Context String
commentField = functionField "comment" \_ _ -> return ""
