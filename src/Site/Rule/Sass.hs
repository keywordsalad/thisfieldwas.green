module Site.Rule.Sass (sassRules) where

import Site.Common

sassRules :: SiteConfig -> Rules ()
sassRules config = do
  sassDependency <- makePatternDependency (fromGlob filePattern)
  rulesExtraDependencies [sassDependency] $
    match "css/main.sass" do
      route $ setExtension "css"
      compile $ sassCompiler config
  where
    filePattern = config ^. siteProviderDirectory ++ "/css/**/*.sass"

sassCompiler :: SiteConfig -> Compiler (Item String)
sassCompiler config =
  fmap compressCss <$> (withItemBody compileSass =<< getResourceString)
  where
    loadPath = config ^. siteProviderDirectory ++ "/css"
    compileSass = unixFilter "sass" args
    args = ["--trace", "--indented", "--stdin", "--load-path", loadPath]
