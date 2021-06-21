module GreenSite.Rule.Robot where

import GreenSite.Common

robotsTxtRules :: SiteConfig -> Rules ()
robotsTxtRules config = do
  create ["robots.txt"] do
    route idRoute
    compile $ robotsTxtCompiler config

robotsTxtCompiler :: SiteConfig -> Compiler (Item String)
robotsTxtCompiler config = do
  makeItem ""
    >>= loadAndApplyTemplate
      (fromFilePath "templates/robots.txt")
      (config ^. siteContext)
