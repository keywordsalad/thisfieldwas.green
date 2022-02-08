module Green.Site.Static where

import Green.Common

static :: Rules ()
static = do
  match "fonts/**" do
    route idRoute
    compile copyFileCompiler
