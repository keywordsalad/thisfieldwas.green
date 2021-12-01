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
    f = lift . (mapM makeLink' <=< getTags . itemIdentifier)
    makeLink' tag = do
      url <- ("/" ++) . fromJust <$> getRoute (makeTagId tag)
      return . renderHtml $
        H.a ! A.href (toValue url) $
          toHtml tag

categoryField :: String -> Context a
categoryField key = field key $ lift . getCategory . itemIdentifier

getCategory :: (MonadMetadata m) => Identifier -> m [String]
getCategory = (filter isCategory <$>) . Hakyll.getCategory
  where
    isCategory = \case
      "_posts" -> False
      "_drafts" -> False
      _ -> True

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
