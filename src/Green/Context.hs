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
import Green.Context.FieldError
import Green.Context.GitCommits
import Green.Util (dropIndex, stripSuffix)

baseContext :: SiteConfig -> Context String
baseContext config = do
  let context =
        mconcat
          [ constField "siteTitle" (config ^. siteTitle),
            siteRootField (config ^. siteRoot),
            linkedInProfileField (config ^. siteLinkedInProfile),
            authorEmailField (config ^. siteAuthorEmail),
            dateFields config,
            gitCommits config,
            trimmedUrlField,
            imgField,
            youtubeField,
            getRouteField,
            commentField,
            defaultContext
          ]
      dependentContexts =
        [ getCodeField,
          linkedTitleField
        ]
   in mconcat (dependentContexts <*> pure context) <> context

authorEmailField :: String -> Context String
authorEmailField = constField "authorEmail"

linkedInProfileField :: String -> Context String
linkedInProfileField = constField "linkedInProfile"

-- | Trims @index.html@ from @$url$@'s
trimmedUrlField :: Context String
trimmedUrlField = mapContext dropIndex (urlField "url")

siteRootField :: String -> Context String
siteRootField = constField "siteRoot"

getCodeField :: Context String -> Context String
getCodeField siteContext' = functionField key f
  where
    key = "getCode"
    f [contentsPath] _ = trimStartEndLines <$> (tryLoad codeId <|> tryLoad fileId)
      where
        codeId = fromFilePath $ "code/" ++ contentsPath
        fileId = fromFilePath contentsPath
        templateId = fromFilePath "_templates/code.md"
        tryLoad = load >=> fmap itemBody . loadAndApplyTemplate templateId siteContext'
        trimStartEndLines = unlines . reverse . dropWhile null . reverse . dropWhile null . lines
    f args item = fieldError key ["contentsPath"] args item

imgField :: Context String
imgField = functionField key f
  where
    key = "img"
    f [imgId, src] = f [imgId, src, ""]
    f [imgId, src, title] = f [imgId, src, title, ""]
    f [imgId, src, title, alt] =
      return . itemBody
        <=< relativizeUrls
        <=< loadAndApplyTemplate
          "_templates/image.html"
          ( mconcat
              [ constField "imgId" imgId,
                constField "imgSrc" src,
                constField "imgTitle" title,
                constField "imgAlt" alt
              ]
          )
    f args = fieldError key expectedArgs args
      where
        expectedArgs = ["imgId", "imgSrc", "imgTitle", "imgAlt"]

youtubeField :: Context String
youtubeField = functionField key f
  where
    key = "youtube"
    f [videoId] = f [videoId, ""]
    f [videoId, asideId] = f [videoId, asideId, ""]
    f [videoId, asideId, title] =
      return . itemBody
        <=< relativizeUrls
        <=< loadAndApplyTemplate
          "_templates/youtube.html"
          ( mconcat
              [ constField "youtubeAsideId" asideId,
                constField "youtubeVideoId" videoId,
                constField "youtubeVideoTitle" title
              ]
          )
    f args = fieldError key expectedArgs args
      where
        expectedArgs = ["youtubeAsideId", "youtubeVideoId", "youtubeVideoTitle"]

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
    f args item = fieldError key ["filePath"] args item

unContextString :: Context String -> String -> [String] -> Item String -> Compiler String
unContextString context key args item =
  unContext context key args item >>= \case
    StringField s -> return s
    _ -> error $ "Got a non-string value in field " ++ key

linkedTitleField :: Context String -> Context String
linkedTitleField context = functionField targetKey f
  where
    targetKey = "linkedTitle"
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
    f args item = fieldError targetKey ["filePath"] args item
