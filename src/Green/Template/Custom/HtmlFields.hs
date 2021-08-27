module Green.Template.Custom.HtmlFields where

import Green.Common
import Green.Template
import Green.Template.Fields
import Green.Util (dropIndex)
import qualified Hakyll as H

-- | Trims @index.html@ from @$url$@'s
trimmedUrlField :: String -> Context String
trimmedUrlField = mapField dropIndex . urlField

siteRootField :: String -> Context String
siteRootField = constField "siteRoot"

codeField :: Context String
codeField = functionField "code" f
  where
    f (contentsPath :: String) _ _ =
      trimStartEndLines <$> (tryLoad codeId <|> tryLoad fileId)
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
    f (imgFields :: Context String) context item = do
      let fields = imgFields <> defaults <> context
      itemBody <$> loadAndApplyTemplate (H.fromFilePath "_templates/image.html") fields item

youtubeField :: Context String
youtubeField = functionField "youtube" f
  where
    defaults = defaultKeys ["id", "video", "title"]
    f (ytFields :: Context String) context item = do
      let fields = ytFields <> defaults <> context
      itemBody <$> loadAndApplyTemplate "_templates/youtube.html" fields item
