module Green.Site.Static where

import Green.Common

static :: Rules ()
static =
  match "CNAME" do
    route idRoute
    compile getResourceBody
