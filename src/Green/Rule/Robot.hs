module Green.Rule.Robot where

import Green.Common
import Green.Config

robotsTxtRules :: SiteConfig -> Rules ()
robotsTxtRules config = do
  create ["robots.txt"] do
    route idRoute
    compile $ robotsTxtCompiler config

robotsTxtCompiler :: SiteConfig -> Compiler (Item String)
robotsTxtCompiler config = do
  makeItem ""
    >>= loadAndApplyTemplate
      (fromFilePath "_templates/robots.txt")
      (config ^. siteContext)
