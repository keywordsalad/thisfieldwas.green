module Green.Site.Download where

import Green.Common

downloads :: Rules ()
downloads = do
  match "downloads/**" do
    route $ setExtension ".txt"
    compile copyFileCompiler
