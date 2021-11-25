module Green.Content.HomePage (homePage) where

import Green.Common
import Green.Content.Blog
import Green.Template.Custom

homePage :: Context String -> Rules ()
homePage context =
  match "index.html" do
    route idRoute
    compile do
      blogContext <- (<> context) <$> recentPostsContext
      getResourceBody
        >>= pageCompiler blogContext
        >>= relativizeUrls
