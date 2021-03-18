module Js (compressJsCompiler) where

import Hakyll
import Text.Jasmine
import qualified Data.ByteString.Lazy.Char8 as C

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s
