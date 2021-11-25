module Green.Content.Download (downloads) where

import Green.Common

downloads :: Rules ()
downloads = do
  match "downloads/**" do
    route $ setExtension ".txt"
    compile copyFileCompiler
