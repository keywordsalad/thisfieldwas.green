module Green.Rule.Css (cssRules) where

import Green.Common

cssRules :: Rules ()
cssRules = do
  scssDependency <- makePatternDependency "css/**"
  rulesExtraDependencies [scssDependency] $
    match "css/main.scss" do
      route $ setExtension "css"
      compile scssCompiler

scssCompiler :: Compiler (Item String)
scssCompiler =
  getResourceString
    >>= withItemBody (unixFilter "sass" ["--trace", "--stdin", "--load-path", "site/css"])
    >>= return . fmap compressCss
