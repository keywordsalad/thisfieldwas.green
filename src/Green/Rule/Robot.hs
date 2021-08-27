module Green.Rule.Robot where

import Green.Common
import Green.Config
import Green.Template
import Green.Template.Custom

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
      (customContext config)
