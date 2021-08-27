module Green.Rule.Index where

import Green.Common
import Green.Config
import Green.Template.Custom

indexRules :: SiteConfig -> Rules ()
indexRules config =
  match "index.html" do
    route idRoute
    compile $ pageCompiler config
