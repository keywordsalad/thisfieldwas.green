module Green.Site.Images where

import Green.Common
import Hakyll

images :: Rules ()
images = do
  match ("images/**" .&&. blacklist) do
    route idRoute
    compile copyFileCompiler
  -- Emit a near-lossless WebP sibling for every PNG/JPEG so templates can offer
  -- a modern, better-compressed source via <picture> (see the `asWebp` field).
  -- The `webp` version keeps a distinct identifier from the copied original.
  match webpConvertible do
    version "webp" do
      route $ setExtension "webp"
      compile do
        image <- getResourceLBS
        withItemBody (unixFilterLBS "cwebp" cwebpArgs) image
  where
    blacklist =
      foldl1 (.||.) . fmap complement $
        [ "*.xcf"
        ]
    webpConvertible =
      "images/**.png" .||. "images/**.jpg" .||. "images/**.jpeg"
    -- Read PNG/JPEG from stdin (`-`), write WebP to stdout (`-o -`).
    cwebpArgs = ["-near_lossless", "60", "-quiet", "-o", "-", "--", "-"]

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
