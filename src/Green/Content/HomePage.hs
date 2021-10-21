module Green.Content.HomePage where

import Green.Common
import Green.Content.Blog.Compiler
import Green.Template.Custom

homePageRules :: Context String -> Rules ()
homePageRules context =
  match "index.html" do
    route idRoute
    compile $ homePageCompiler context

homePageCompiler :: Context String -> Compiler (Item String)
homePageCompiler context = do
  blogContext <- (<> context) <$> recentPostsContext
  pageCompiler blogContext =<< getResourceBody
