module Green.Site.HomePage where

import Green.Common
import Green.Site.Blog
import Green.Template.Custom
import Hakyll (recentFirst)
import Green.Hakyllbars as HB

homePage :: Context String -> Rules ()
homePage siteContext =
  match "index.html" do
    route idRoute
    compile do
      recentPosts <- fmap (take 5) $ recentFirst =<< loadPublishedPosts
      getResourceBody >>= applyTemplates do
        applyContext $
          itemsField "recentPosts" siteContext recentPosts
            <> teaserField "teaser" "content"
            <> siteContext
        applyContent
        applyLayout
