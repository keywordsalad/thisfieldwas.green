module Green.Site.HomePage where

import Green.Common
import Green.Config
import Green.Site.Blog
import Green.Template.Custom
import Hakyll (recentFirst)

homePage :: SiteConfig -> Context String -> Rules ()
homePage config siteContext =
  match "index.html" do
    let preview = config ^. sitePreview
    route idRoute
    compile do
      recentPosts <- fmap (take 5) $ recentFirst =<< loadPublishedPosts preview
      getResourceBody >>= applyTemplates do
        applyContext $
          itemsField "recentPosts" siteContext recentPosts
            <> teaserField "teaser" "content"
            <> siteContext
        applyContent
        applyLayout
