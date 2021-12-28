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

imgField :: Context String
imgField = functionField "img" f
  where
    defaults = defaultKeys ["id", "src", "title", "alt"]
    f (imgFields :: Context String) =
      tplWithContext (imgFields <> defaults) do
        loadAndApplyTemplate "_templates/image.html"
        itemBody <$> tplPopItem

youtubeField :: Context String
youtubeField = functionField "youtube" f
  where
    defaults = defaultKeys ["id", "video", "title"]
    f (ytFields :: Context String) =
      tplWithContext (ytFields <> defaults) do
        loadAndApplyTemplate "_templates/youtube.html"
        itemBody <$> tplPopItem

escapeHtmlField :: Context String
escapeHtmlField = functionField "escapeHtml" f
  where
    f = return . escapeHtml

escapeHtmlUriField :: Context String
escapeHtmlUriField = functionField "escapeHtmlUri" f
  where
    f = return . escapeHtml . escapeURIString isUnescapedInURI
