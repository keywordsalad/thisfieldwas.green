module Green.Site.Js where

import qualified Data.ByteString.Lazy.Char8 as C
import Green.Common
import Green.Config
import Text.Jasmine

js :: SiteConfig -> Rules ()
js config =
  match "js/**.js" do
    route idRoute
    compile do
      s <- getResourceString
      return $ itemSetBody (processJS $ itemBody s) s
  where
    processJS
      | config ^. siteDebug . debugInflateJs = id
      | otherwise = minifyJS
    minifyJS = C.unpack . minify . C.pack
