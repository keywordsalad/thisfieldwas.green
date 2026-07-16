module Green.Site.Css where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Green.Common
import Green.Config
import Hakyll (compressCss, fromGlob, unixFilter)

scss :: SiteConfig -> Rules ()
scss siteConfig = do
  scssDependency <- makePatternDependency "css/**"
  -- `$grass-icon` is inlined into the compiled CSS from the source SVG (see
  -- grassIconVar), so a change to that file must invalidate the stylesheets.
  iconDependency <- makePatternDependency (fromGlob grassIconSource)
  rulesExtraDependencies [scssDependency, iconDependency] $
    match ("css/main.scss" .||. "css/pages/*.scss" .||. "css/posts/*.scss") do
      route $ setExtension "css"
      compile do
        -- Prepend `$grass-icon`, read fresh from the source SVG, so the inlined
        -- data URI is always derived from site/images/grass.svg rather than a
        -- hand-maintained copy. Every entry point opens with `@import`, so a
        -- leading variable declaration is valid (no `@use`-first violation).
        prelude <- grassIconVar
        css <- withItemBody (compileSass prelude) =<< getResourceString
        if siteConfig ^. siteDebug . debugInflateCss
          then return css
          else return $ compressCss <$> css
  where
    compileSass prelude =
      unixFilter "sass" ["--trace", "--stdin", "--load-path", "site/css"] . (prelude <>)

-- | The brand grass icon within the Hakyll provider directory (config
-- `provider-directory: site`). Used both as a build dependency and as the
-- source that gets inlined into the CSS.
grassIconSource :: FilePath
grassIconSource = "images/grass.svg"

-- | A `$grass-icon` SCSS declaration binding the variable to the brand SVG
-- inlined as a `data:` URI, read fresh from the provider directory at build
-- time. Dart Sass can't read arbitrary files itself, so we compute the value
-- here and prepend it to the SCSS before compiling; deriving it on every build
-- keeps the inlined icon from ever drifting from the source file.
grassIconVar :: Compiler String
grassIconVar = do
  svg <- unsafeCompiler (TIO.readFile ("site" </> grassIconSource))
  return ("$grass-icon: url(\"" <> T.unpack (svgDataUri svg) <> "\");\n")

-- | Encode an SVG document as a compact, URL-encoded `data:` URI suitable for a
-- CSS `url()` value: drop the XML prolog, collapse whitespace, single-quote
-- attributes (so the whole thing can be wrapped in double quotes), and
-- percent-encode the characters that would otherwise break the URI — notably
-- `#` in the fill colors.
svgDataUri :: T.Text -> T.Text
svgDataUri svg = "data:image/svg+xml," <> percentEncode collapsed
  where
    body = snd (T.breakOn "<svg" svg)
    collapsed = T.unwords (T.words (T.replace "\"" "'" body))
    -- `%` must be encoded first so the escapes introduced below aren't re-encoded.
    percentEncode =
      T.replace "&" "%26"
        . T.replace ">" "%3E"
        . T.replace "<" "%3C"
        . T.replace "#" "%23"
        . T.replace "%" "%25"
