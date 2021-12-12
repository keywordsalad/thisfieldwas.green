module Green.Site.Css where

import Green.Common
import Green.Config
import Hakyll (compressCss, unixFilter)

scss :: SiteConfig -> Rules ()
scss siteConfig = do
  scssDependency <- makePatternDependency "css/**"
  rulesExtraDependencies [scssDependency] $
    match "css/main.scss" do
      route $ setExtension "css"
      compile do
        css <- withItemBody compileSass =<< getResourceString
        if siteConfig ^. siteDebug . debugRawCss
          then return css
          else return $ compressCss <$> css
  where
    compileSass = unixFilter "sass" ["--trace", "--stdin", "--load-path", "site/css"]
