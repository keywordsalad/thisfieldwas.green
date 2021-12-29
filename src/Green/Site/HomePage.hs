module Green.Site.HomePage where

import Green.Common
import Green.Site.Blog
import Green.Template.Custom
import Hakyll (recentFirst)

homePage :: Context String -> Rules ()
homePage siteContext =
  match "index.html" do
    route idRoute
    compile do
      recentPosts <- fmap (take 5) $ recentFirst =<< loadPublishedPosts
      getResourceBody >>= applyTemplates do
        applyContext $
          constField "recentPosts" (itemListValue siteContext recentPosts)
            <> teaserField "teaser" publishedPostsSnapshot
            <> siteContext
        applyContent
        applyLayout
