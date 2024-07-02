module Green.Template.Tags
  ( module Green.Template.Tags,
    Tags,
    buildTags,
  )
where

import Data.Char (toLower)
import Green.Common
import Green.Util
import Hakyll (Tags, buildTags, getTags, renderTagCloudWith)
import Hakyll qualified
import Green.Hakyllbars as HB

normalizeTag :: String -> String
normalizeTag tag = toLower <$> sanitized
  where
    sanitized =
      wordsToKebab $ camelToKebab <$> words tag

makeTagId :: String -> Identifier
makeTagId = Hakyll.fromCapture "tags/*.html" . normalizeTag

tagsField :: String -> Context a
tagsField key = field key $ lift . getTags . itemIdentifier

tagLinksFieldWith :: String -> (Identifier -> Compiler [String]) -> Context a
tagLinksFieldWith key f = field key f'
  where
    f' item = lift do
      tags <- f $ itemIdentifier item
      links <- mapM makeLink' tags
      return $ intercalate ", " links
    makeLink' tag =
      getRoute (makeTagId tag) >>= \case
        Just url -> return $ "<a class=\"tag\" href=\"/" ++ dropIndex url ++ "\">" ++ tag ++ "</a>"
        Nothing -> return $ "<span class=\"tag\">" ++ tag ++ "</span>"

tagLinksField :: String -> Context a
tagLinksField key = tagLinksFieldWith key getTags

renderTagCloud :: Tags -> Compiler String
renderTagCloud = renderTagCloudWith makeLink unwords minSize maxSize
  where
    minSize = 1.0
    maxSize = 2.0

makeLink :: Double -> Double -> String -> String -> Int -> Int -> Int -> String
makeLink _minSize _maxSize tag url count minCount maxCount =
  mconcat
    [ "<a",
      " data-count=\"" ++ show count ++ "\"",
      " class=\"tag\"",
      " style=\""
        ++ mconcat
          [ ";--count:" ++ show count,
            ";--min-count:" ++ show minCount,
            ";--max-count:" ++ show maxCount
          ]
        ++ "\"",
      " href=\"" ++ dropIndex url ++ "\"",
      ">" ++ tag ++ "</a>"
    ]
