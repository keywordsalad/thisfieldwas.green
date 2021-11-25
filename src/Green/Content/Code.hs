module Green.Content.Code (code) where

import Green.Common

code :: Rules Dependency
code = do
  let path = "code/**"
  match path do
    route idRoute
    compile getResourceBody
  makePatternDependency path
