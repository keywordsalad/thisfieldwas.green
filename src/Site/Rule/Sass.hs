module Site.Rule.Sass (sassRules) where

import Site.Common

sassRules :: Rules ()
sassRules = do
  sassDependency <- makePatternDependency "css/**.sass"
  rulesExtraDependencies [sassDependency] $
    match "css/main.sass" do
      route $ setExtension "css"
      compile sassCompiler

sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString
  >>= withItemBody (unixFilter "sass" args)
  >>= return . fmap compressCss
  where
    args = ["--trace", "--indented", "--stdin", "--load-path", "css"]
