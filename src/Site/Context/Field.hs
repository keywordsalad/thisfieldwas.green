module Site.Context.Field
  ( siteRootField,
    includeCodeField,
    imgField,
    youtubeField,
    routeToField,
    commentField,
    demoteHeadersByField,
  )
where

import Control.Monad ((>=>))
import Hakyll hiding (demoteHeaders)
import Lens.Micro
import Site.Config (SiteConfig, siteRoot)
import Site.Util

siteRootField :: SiteConfig -> Context String
siteRootField config = constField "site-root" (config ^. siteRoot)

includeCodeField :: Context String
includeCodeField = functionField fieldName f
  where
    fieldName = "include-code"
    f [lexer, contentsPath] _ = wrapCode <$> body
      where
        wrapCode code = "``` " ++ lexer ++ "\n" ++ code ++ "\n```"
        body = loadSnapshotBody item "code"
        item = fromFilePath $ "code/" ++ contentsPath
    f _ item = error $ fieldName ++ " needs a filepath and a lexer " ++ show (itemIdentifier item)

imgField :: Context String
imgField = functionField fieldName f
  where
    fieldName = "img"
    f [path] = f [path, "untitled"]
    f [path, title] = f [path, title, title]
    f [path, title, alt] =
      loadAndApplyTemplate
        "partials/image.html"
        ( constField "img-src" path
            <> constField "img-title" title
            <> constField "img-alt" alt
        )
        >=> return . itemBody
    f _ = \item -> error $ fieldName ++ " needs an image source and optionally a title " ++ show (itemIdentifier item)

youtubeField :: Context String
youtubeField = functionField fieldName f
  where
    fieldName = "youtube"
    f [videoId] = f [videoId, "YouTube video player"]
    f [videoId, title] =
      loadAndApplyTemplate
        "partials/youtube.html"
        ( constField "youtube-id" videoId
            <> constField "youtube-title" title
        )
        >=> return . itemBody
    f _ = \item -> error $ fieldName ++ " needs a youtube video id and optionally a title " ++ show (itemIdentifier item)

routeToField :: Context String
routeToField = functionField fieldName f
  where
    fieldName = "route-to"
    f [filePath] item = do
      getRoute id' >>= \case
        Just route' -> return $ "/" ++ stripSuffix "index.html" route'
        Nothing -> error $ fieldName ++ " in " ++ show fromId ++ ": no route to " ++ show id'
      where
        id' = fromFilePath filePath
        fromId = itemIdentifier item
    f _ item = error $ fieldName ++ " needs a filePath " ++ show (itemIdentifier item)

commentField :: Context String
commentField = functionField "comment" \_ _ -> return ""

demoteHeadersByField :: Context String
demoteHeadersByField = functionField fieldName f
  where
    fieldName = "demote-headers-by"
    f [amount, content] _ = return $ demoteHeadersBy (read amount :: Int) content
    f _ item = error $ fieldName ++ " requires a reduction amount and content " ++ show (itemIdentifier item)
