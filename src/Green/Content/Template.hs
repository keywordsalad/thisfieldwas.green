module Green.Content.Template (templates) where

import Green.Common
import Green.Template

templates :: Rules ()
templates = do
  match "_layouts/**" $ compile templateCompiler
  match "_partials/**" $ compile templateCompiler
  match "_templates/**" $ compile templateCompiler
