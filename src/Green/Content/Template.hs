module Green.Content.Template where

import Green.Template
import Green.Common

templateRules :: Rules ()
templateRules = do
  match "_layouts/**" $ compile templateCompiler
  match "_partials/**" $ compile templateCompiler
  match "_templates/**" $ compile templateCompiler
