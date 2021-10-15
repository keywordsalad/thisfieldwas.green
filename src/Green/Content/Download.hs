module Green.Content.Download where

import Green.Common
import qualified Hakyll as H

downloadRules :: Rules ()
downloadRules = do
  match "downloads/**" do
    route $ setExtension ".txt"
    compile H.copyFileCompiler
