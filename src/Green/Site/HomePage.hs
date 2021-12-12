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
      let context =
            constField "recentPosts" (itemListValue siteContext recentPosts)
              <> teaserField "teaser" publishedPostsSnapshot
              <> siteContext
      getResourceBody
        >>= contentCompiler context
        >>= layoutCompiler context
        >>= relativizeUrls
