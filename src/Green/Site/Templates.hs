module Green.Site.Templates where

import Green.Common
import Green.Template
import Hakyll.Core.Identifier.Pattern ((.||.))

templates :: Rules Dependency
templates = do
  let templatePattern =
        "_layouts/**"
          .||. "_partials/**"
          .||. "_templates/**"
  match templatePattern do
    compile getResourceTemplate
  makePatternDependency templatePattern
