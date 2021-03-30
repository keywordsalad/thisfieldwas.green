module Site.Rule.Sass (sassRules) where

import Hakyll

sassRules :: Rules ()
sassRules = do
  sassDependency <- makePatternDependency "css/**.sass"
  rulesExtraDependencies [sassDependency] $
    match "css/main.sass" do
      route $ setExtension "css"
      compile sassCompiler

sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString
  >>= withItemBody (unixFilter "sass" ["--trace", "--stdin", "--load-path", "site-src/css"])
  >>= return . fmap compressCss
