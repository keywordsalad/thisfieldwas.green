module Green.Template.Tags
  ( module Green.Template.Tags,
    Tags,
    buildTags,
    getTags,
  )
where

import Green.Common
import Green.Template.Context
import Hakyll (MonadMetadata, Tags, buildTags, getTags, renderTagCloudWith)
import qualified Hakyll

makeTagId :: String -> Identifier
makeTagId = Hakyll.fromCapture "blog/tags/*.html"

tagsField :: String -> Context a
tagsField key = field key $ lift . getTags . itemIdentifier

tagLinksField :: String -> Context a
tagLinksField key = field key f
  where
    f item = lift do
      tags <- getTags $ itemIdentifier item
      links <- mapM makeLink' tags
      return $ unwords links
    makeLink' tag =
      getRoute (makeTagId tag) >>= \case
        Just url -> return $ "<a href=\"/" ++ url ++ "\">" ++ tag ++ "</a>"
        Nothing -> return tag

makeCategoryId :: String -> Identifier
makeCategoryId = Hakyll.fromCapture "blog/categories/*.html"

categoriesField :: String -> Context a
categoriesField key = field key $ lift . getCategory . itemIdentifier

getCategory :: (MonadMetadata m) => Identifier -> m [String]
getCategory = (filter isCategory <$>) . Hakyll.getCategory
  where
    isCategory = not . (`elem` sourceDirs)
    sourceDirs = ["_posts", "_drafts"]

buildCategories :: (MonadMetadata m) => Pattern -> (String -> Identifier) -> m Tags
buildCategories = Hakyll.buildTagsWith getCategory

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
      " href=\"" ++ url ++ "\"",
      ">" ++ tag ++ "</a>"
    ]
