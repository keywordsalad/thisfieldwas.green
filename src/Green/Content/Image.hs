module Green.Content.Image where

import Green.Common
import qualified Hakyll as H

imageRules :: Rules ()
imageRules =
  match "images/**" do
    route idRoute
    compile H.copyFileCompiler

-- imageRules :: SiteConfig -> Rules ()
-- imageRules config = do
--   let x =
--         config .^ siteDisplayFormat . displayImageWidths <&> \width ->
--           group ("images-" ++ show width) do
--             match "images/**" do
--               route $ gsubRoute ".\\w+$" (\s -> "-" ++ show width ++ s)
--               compile $ imageCompiler width
--   match "images/**" do
--     route idRoute
--     compile $ imageCompiler (config .^ siteDisplayFormat . displayImageWidths)

-- imageCompiler :: [Int] -> Item a
-- imageCompiler sizes = do
