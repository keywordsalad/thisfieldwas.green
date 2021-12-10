module Green.Content.HomePage (homePage) where

import Green.Common
import Green.Config
import Green.Content.Blog
import Green.Template.Custom
import Hakyll (recentFirst)

homePage :: SiteConfig -> Context String -> Rules ()
homePage config siteContext =
  match "index.html" do
    route idRoute
    compile do
      posts <- fmap (take 5) $ recentFirst =<< loadPublishedPosts config
      let context =
            constField "previousPosts" (itemListValue siteContext posts)
              <> teaserField "teaser" publishedPostsSnapshot
              <> siteContext
      getResourceBody
        >>= contentCompiler context
        >>= layoutCompiler context
        >>= relativizeUrls
