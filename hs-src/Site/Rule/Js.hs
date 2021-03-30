module Site.Rule.Js (jsRules) where

import qualified Data.ByteString.Lazy.Char8 as C
import Hakyll
import Text.Jasmine

jsRules :: Rules ()
jsRules =
  match "js/**.js" do
    route idRoute
    compile compressJsCompiler

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s
