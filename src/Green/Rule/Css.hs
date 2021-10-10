module Green.Rule.Css (scssRules) where

import Green.Common
import Green.Config

scssRules :: SiteConfig -> Rules ()
scssRules siteConfig = do
  scssDependency <- makePatternDependency "css/**"
  rulesExtraDependencies [scssDependency] $
    match "css/main.scss" do
      route $ setExtension "css"
      compile $ scssCompiler siteConfig

scssCompiler :: SiteConfig -> Compiler (Item String)
scssCompiler siteConfig = do
  css <- getResourceString >>= withItemBody compileSass
  if siteConfig ^. siteDebug . debugRawCss
    then return css
    else return $ compressCss <$> css
  where
    compileSass = unixFilter "sass" ["--trace", "--stdin", "--load-path", "site/css"]
