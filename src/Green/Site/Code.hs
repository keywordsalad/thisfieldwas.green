module Green.Site.Code where

import Green.Common

code :: Rules ()
code =
  match "code/**" do
    route idRoute
    compile getResourceBody
