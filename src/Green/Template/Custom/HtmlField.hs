module Green.Template.Custom.HtmlField where

import Green.Common
import Green.Template
import Green.Util (dropIndex)
import Network.URI (escapeURIString, isUnescapedInURI)

-- | Trims @index.html@ from @$url$@'s
trimmedUrlField :: String -> Context String
trimmedUrlField = mapField dropIndex . urlField

siteRootField :: String -> Context String
siteRootField = constField "siteRoot"

codeField :: Context String
codeField = functionField "code" f
  where
    f (contentsPath :: String) =
      lift $ trimStartEndLines <$> (tryLoad codeId <|> tryLoad fileId)
      where
        codeId = fromFilePath $ "code/" ++ contentsPath
        fileId = fromFilePath contentsPath
        tryLoad = fmap itemBody . load
        trimStartEndLines =
          unlines
            . reverse
            . dropWhile null
            . reverse
            . dropWhile null
            . lines

imageFigureField :: Context String
imageFigureField = functionField "imageFigure" f
  where
    defaults = defaultKeys ["id", "src", "title", "alt"]
    f (imageFigureFields :: Context String) =
      tplWithContext (imageFigureFields <> defaults) do
        applyTemplate "_templates/image-figure.html"
        tplPopBody

youtubeField :: Context String
youtubeField = functionField "youtube" f
  where
    defaults = defaultKeys ["id", "video", "title"]
    f (ytFields :: Context String) =
      tplWithContext (ytFields <> defaults) do
        applyTemplate "_templates/youtube.html"
        tplPopBody

escapeHtmlField :: Context String
escapeHtmlField = functionField "escapeHtml" f
  where
    f = return . escapeHtml

escapeHtmlUriField :: Context String
escapeHtmlUriField = functionField "escapeHtmlUri" f
  where
    f = return . escapeHtml . escapeURIString isUnescapedInURI
