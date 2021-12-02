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
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
        Just url ->
          return . renderHtml $
            H.a ! A.href (toValue $ "/" ++ url) $
              toHtml tag
        Nothing ->
          return tag

categoryField :: String -> Context a
categoryField key = field key $ lift . getCategory . itemIdentifier

getCategory :: (MonadMetadata m) => Identifier -> m [String]
getCategory = (filter isCategory <$>) . Hakyll.getCategory
  where
    isCategory = not . (`elem` sourceDirs)
    sourceDirs = ["_posts", "_drafts"]

buildCategories :: (MonadMetadata m) => Pattern -> (String -> Identifier) -> m Tags
buildCategories = Hakyll.buildTagsWith getCategory

renderTagCloud :: Double -> Double -> Tags -> Compiler String
renderTagCloud = renderTagCloudWith makeLink unwords

makeLink :: Double -> Double -> String -> String -> Int -> Int -> Int -> String
makeLink minSize maxSize tag url count minCount maxCount =
  let diff = 1 + fromIntegral maxCount - fromIntegral minCount
      relative = (fromIntegral count - fromIntegral minCount) / diff
      size = floor ((minSize + relative * (maxSize - minSize)) * 100) :: Int
   in renderHtml $
        H.a ! A.style (toValue $ "font-size: " ++ show size ++ "%")
          ! A.href (toValue url)
          $ toHtml (tag ++ " (" ++ show count ++ ")")
