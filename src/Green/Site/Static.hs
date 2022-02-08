module Green.Site.Static where

import Green.Common

static :: Rules ()
static = do
<<<<<<< HEAD
  match "fonts/**" do
=======
  match "fonts/*" $ do
>>>>>>> e8e0819 (Edits, changing code style to inherit context)
    route idRoute
    compile copyFileCompiler
