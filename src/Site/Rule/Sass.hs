module Site.Rule.Sass (sassRules) where

import Site.Common

sassRules :: Rules ()
sassRules = do
  sassDependency <- makePatternDependency "css/**"
  rulesExtraDependencies [sassDependency] $
    match "css/main.sass" do
      route $ setExtension "css"
      compile sassCompiler

sassCompiler :: Compiler (Item String)
sassCompiler =
  getResourceString
    >>= withItemBody (unixFilter "sass" ["--trace", "--stdin", "--load-path", "css"])
    >>= return . fmap compressCss
