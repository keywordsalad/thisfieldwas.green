module Green.Content.Robot where

import Green.Common
import Green.Template

robotsTxtRules :: Context String -> Rules ()
robotsTxtRules context = do
  create ["robots.txt"] do
    route idRoute
    compile $ robotsTxtCompiler context

robotsTxtCompiler :: Context String -> Compiler (Item String)
robotsTxtCompiler context = do
  makeItem ""
    >>= loadAndApplyTemplate
      (fromFilePath "_templates/robots.txt")
      context
