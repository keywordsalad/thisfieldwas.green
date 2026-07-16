module Green.Site.Css where

import Data.Text qualified as T
import Green.Common
import Green.Config
import Green.Site.Images (grassIconSource, svgDataUri)
import Hakyll (compressCss, unixFilter)

scss :: SiteConfig -> Rules ()
scss siteConfig = do
  scssDependency <- makePatternDependency "css/**"
  rulesExtraDependencies [scssDependency] $
    match ("css/main.scss" .||. "css/pages/*.scss" .||. "css/posts/*.scss") do
      route $ setExtension "css"
      compile do
        -- Load the brand SVG through Hakyll (dependency tracked, no
        -- unsafeCompiler) and prepend `$grass-icon` as an inlined data URI.
        -- Every entry point opens with `@import`, so a leading variable
        -- declaration is valid (no `@use`-first violation).
        svg <- itemBody <$> load (fromFilePath grassIconSource)
        let prelude = "$grass-icon: url(\"" <> T.unpack (svgDataUri (T.pack svg)) <> "\");\n"
        css <- withItemBody (compileSass prelude) =<< getResourceString
        if siteConfig ^. siteDebug . debugInflateCss
          then return css
          else return $ compressCss <$> css
  where
    compileSass prelude =
      unixFilter "sass" ["--trace", "--stdin", "--load-path", "site/css"] . (prelude <>)
