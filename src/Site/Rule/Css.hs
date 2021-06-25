module Site.Rule.Css (cssRules) where

import Site.Common

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
    >>= withItemBody (unixFilter "sass" ["--trace", "--stdin", "--load-path", "css"])
    >>= return . fmap compressCss
