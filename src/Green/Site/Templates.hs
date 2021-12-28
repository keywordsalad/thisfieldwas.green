module Green.Site.Templates where

import Green.Common
import Green.Template
import Hakyll.Core.Identifier.Pattern ((.||.))

templates :: Rules Dependency
templates = do
  match templatePattern do
    compile getResourceTemplate
  makePatternDependency templatePattern
  where
    templatePattern =
      "_layouts/**"
        .||. "_partials/**"
        .||. "_templates/**"
