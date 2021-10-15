module Green.Content.Code where

import Green.Common

codeRules :: Rules Dependency
codeRules = do
  let path = "code/**"
  match path do
    route idRoute
    compile getResourceBody
  makePatternDependency path
