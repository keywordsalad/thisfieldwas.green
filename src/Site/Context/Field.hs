module Site.Context.Field where

import Site.Common
import Site.Util

includeCodeField :: Context String
includeCodeField = functionField "include-code" f
  where
    f (lexer:contentsPath:[]) _ = fmap wrapCode body
      where
        wrapCode code = "```" ++ lexer ++ "\n" ++ code ++ "\n```"
        body = loadSnapshotBody item "code"
        item = fromFilePath $ "code/" ++ contentsPath
    f _ item = error $ "codeIncludeField needs a filepath and a lexer " ++ show (itemIdentifier item)

imgField :: Context String
imgField = functionField "img" f
  where
    f (path:[]) = f (path:"untitled":[])
    f (path:title:[]) = f (path:title:title:[])
    f (path:title:alt:[]) =
      loadAndApplyTemplate "templates/image.html"
        (constField "img-src" path
         <> constField "img-title" title
         <> constField "img-alt" alt)
      >=> relativizeUrls
      >=> return . itemBody
    f _ = \item -> error $ "imgField needs an image source and optionally a title " ++ show (itemIdentifier item)

youtubeField :: Context String
youtubeField = functionField "youtube" f
  where
    f (videoId:[]) = f (videoId:"YouTube video player":[])
    f (videoId:title:[]) =
      loadAndApplyTemplate "templates/youtube.html"
        (constField "youtube-id" videoId
         <> constField "youtube-title" title)
      >=> relativizeUrls
      >=> return . itemBody
    f _ = \item -> error $ "youtubeField needs a youtube video id and optionally a title " ++ show (itemIdentifier item)

routeToField :: Context String
routeToField = functionField "route-to" f
  where
    f (filePath:[]) item = do
      getRoute id' >>= \case
        Just r  -> return ("/" ++ stripIndex r)
        Nothing -> error $ "routeField in " ++ show fromId ++ ": no route to " ++ show id'
      where
        id' = fromFilePath filePath
        fromId = itemIdentifier item
    f _ item = error $ "routeField needs a filePath " ++ show (itemIdentifier item)
