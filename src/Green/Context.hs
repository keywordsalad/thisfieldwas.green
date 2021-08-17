module Green.Context
  ( module Green.Context,
    module Green.Context.DateFields,
    module Green.Context.GitCommits,
  )
where

import Data.String.Utils (endswith)
import Green.Common
import Green.Config
import Green.Context.DateFields
import Green.Context.GitCommits
import Green.Template
import Green.Util (dropIndex, stripSuffix)
import Hakyll.Web.Html (escapeHtml)

baseContext :: SiteConfig -> Context String
baseContext config =
  mconcat
    [ constField "siteTitle" (config ^. siteTitle),
      constField "siteRoot" (config ^. siteRoot),
      constField "linkedInProfile" (config ^. siteLinkedInProfile),
      constField "authorEmail" (config ^. siteAuthorEmail),
      trimmedUrlField "url",
      dateFields config,
      gitCommits config,
      imgField,
      youtubeField,
      getRouteField,
      defaultContext,
      linkedTitleField,
      getCodeField
    ]

-- | Trims @index.html@ from @$url$@'s
trimmedUrlField :: String -> Context String
trimmedUrlField = mapField dropIndex . urlField

siteRootField :: String -> Context String
siteRootField = constField "siteRoot"

getCodeField :: Context String
getCodeField = constField "getCode" f
  where
    f :: FunctionValue String String String
    f contentsPath _ _ = trimStartEndLines <$> (tryLoad codeId <|> tryLoad fileId)
      where
        codeId = fromFilePath $ "code/" ++ contentsPath
        fileId = fromFilePath contentsPath
        tryLoad = fmap itemBody . compilePandoc <=< load
        trimStartEndLines =
          unlines
            . reverse
            . dropWhile null
            . reverse
            . dropWhile null
            . lines

imgField :: Context String
imgField = constField "img" f
  where
    templatePath = "_templates/image.html"
    f :: FunctionValue String String String
    f src context _ = do
      let context' = constField "src" src <> context
      template <- load templatePath
      applied <- applyAsTemplate context' template
      return $ itemBody applied

youtubeField :: Context String
youtubeField = constField "youtube" f
  where
    templatePath = "_templates/youtube.html"
    f :: FunctionValue String String String
    f videoId context _ = do
      let context' = constField "videoId" videoId <> context
      template <- load templatePath
      rendered <- applyAsTemplate context' template
      return $ itemBody rendered

getRouteField :: Context String
getRouteField = constField "route" f
  where
    f :: FunctionValue String String String
    f filePath _ _ = do
      let id' = fromFilePath filePath
      getRoute id' >>= \case
        Just r -> return $ "/" ++ stripSuffix "index.html" r
        Nothing -> error $ "no route to " ++ show id'

linkedTitleField :: Context String
linkedTitleField = constField "linkedTitle" f
  where
    f :: FunctionValue String String String
    f filePath context _ = do
      linkedItem <- load (fromFilePath filePath)
      makeLink <$> getField "title" linkedItem <*> getField "url" linkedItem
      where
        getField key = intoString <=< unContext context key
        makeLink title url
          | endswith ".html" filePath = "<a href=\"" ++ url ++ "\">" ++ escapeHtml title ++ "</a>"
          | endswith ".md" filePath = "[" ++ escapeHtml title ++ "](" ++ url ++ ")"
          | otherwise = title ++ " <" ++ url ++ ">"
