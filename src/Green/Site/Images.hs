module Green.Site.Images where

import Data.Text qualified as T
import Green.Common
import Hakyll

images :: Rules ()
images = do
  match grassIconPattern do
    route idRoute
    compile getResourceString
  match ("images/**" .&&. blacklist .&&. complement grassIconPattern) do
    route idRoute
    compile copyFileCompiler
  -- Emit a WebP sibling for every raster image so templates can offer a modern,
  -- better-compressed source via <picture> (see the `asWebp` field). The `webp`
  -- version keeps a distinct identifier from the copied original. Still images
  -- go through cwebp (near-lossless); animated GIFs need gif2webp, which cwebp
  -- can't read.
  match stillConvertible do
    version "webp" do
      route $ setExtension "webp"
      compile $ toWebp "cwebp" cwebpArgs
  match "images/**.gif" do
    version "webp" do
      route $ setExtension "webp"
      compile $ toWebp "gif2webp" gif2webpArgs
  where
    -- Pipe the resource bytes through the encoder, reading stdin (`-`) and
    -- writing WebP to stdout (`-o -`).
    toWebp cmd args = getResourceLBS >>= withItemBody (unixFilterLBS cmd args)
    blacklist =
      foldl1 (.||.) . fmap complement $
        [ "*.xcf"
        ]
    stillConvertible =
      "images/**.png" .||. "images/**.jpg" .||. "images/**.jpeg"
    cwebpArgs = ["-near_lossless", "60", "-quiet", "-o", "-", "--", "-"]
    gif2webpArgs = ["-lossy", "-q", "50", "-quiet", "-o", "-", "--", "-"]

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

grassIconSource :: String
grassIconSource = "images/grass.svg"

grassIconPattern :: Pattern
grassIconPattern = fromGlob grassIconSource

-- | Encode an SVG document as a compact, URL-encoded `data:` URI suitable for a
-- CSS `url()` value: drop the XML prolog, collapse whitespace, single-quote
-- attributes (so the whole thing can be wrapped in double quotes), and
-- percent-encode the characters that would otherwise break the URI — notably
-- `#` in the fill colors.
svgDataUri :: T.Text -> T.Text
svgDataUri svg = "data:image/svg+xml," <> percentEncode collapsed
  where
    body = snd $ T.breakOn "<svg" svg
    collapsed = T.unwords $ T.words $ T.replace "\"" "'" body
    -- `%` must be encoded first so the escapes introduced below aren't re-encoded.
    percentEncode =
      T.replace "&" "%26"
        . T.replace ">" "%3E"
        . T.replace "<" "%3C"
        . T.replace "#" "%23"
        . T.replace "%" "%25"
