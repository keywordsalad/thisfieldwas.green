module Green.Content.Download where

import Green.Common

downloadRules :: Rules ()
downloadRules = do
  match "downloads/**" do
    route $ setExtension ".txt"
    compile copyFileCompiler
