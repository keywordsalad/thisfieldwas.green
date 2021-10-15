module Green.Content.HomePage where

import Green.Common
import Green.Template.Custom

homePageRules :: Context String -> Rules ()
homePageRules context =
  match "index.html" do
    route idRoute
    compile $ pageCompiler context =<< getResourceBody
