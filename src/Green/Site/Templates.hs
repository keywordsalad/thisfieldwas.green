module Green.Site.Templates where

import Green.Common
import Green.Template
import Hakyll.Core.Identifier.Pattern ((.||.))

templates :: Rules ()
templates =
  match templatePattern do
    compile getResourceTemplate
  where
    templatePattern =
      "_layouts/**"
        .||. "_partials/**"
        .||. "_templates/**"
