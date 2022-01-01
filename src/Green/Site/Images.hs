module Green.Site.Images where

import Green.Common
import Hakyll ((.||.))

images :: Rules ()
images =
  match ("images/**/*.png" .||. "images/**/*.jpg" .||. "images/**/*.gif") do
    route idRoute
    compile copyFileCompiler

-- images :: SiteConfig -> Rules ()
-- images config = do
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
