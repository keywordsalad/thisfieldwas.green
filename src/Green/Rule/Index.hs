module Green.Rule.Index where

import Green.Common
import Green.Config

indexRules :: SiteConfig -> Rules ()
indexRules config =
  match "index.html" do
    route idRoute
    compile $ indexCompiler config

indexCompiler :: SiteConfig -> Compiler (Item String)
indexCompiler _ = makeItem "index"
