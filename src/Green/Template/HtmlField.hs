module Green.Template.HtmlField where

import Green.Common
import Green.Util (dropIndex)
import Green.Hakyllbars

-- | Trims @index.html@ from @$url$@'s
trimmedUrlField :: String -> Context String
trimmedUrlField key = mapField dropIndex $ urlField key "siteRoot"

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
