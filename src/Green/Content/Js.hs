module Green.Content.Js (js) where

import qualified Data.ByteString.Lazy.Char8 as C
import Hakyll
import Text.Jasmine

js :: Rules ()
js =
  match "js/**.js" do
    route idRoute
    compile do
      let minifyJS = C.unpack . minify . C.pack . itemBody
      s <- getResourceString
      return $ itemSetBody (minifyJS s) s
