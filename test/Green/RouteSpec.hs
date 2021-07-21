module Green.RouteSpec where

import Green.Route
import Green.TestSupport

spec :: Spec
spec = do
  around (withDefaultTestEnv `providing` runRouteSpec) do
    describe "indexRoute" do
      runRouteExamples indexRoute $
        [ ("info/tag-cloud.html", Just "info/tag-cloud/index.html"),
          ("contact.html", Just "contact/index.html"),
          ("contact/index.html", Just "contact/index.html")
        ]
    describe "subPrefixRoute" do
      runRouteExamples (subPrefixRoute "pages/" "potatoes/") $
        [ ("pages/contact.md", Just "potatoes/contact.md"),
          ("pages/archives.html", Just "potatoes/archives.html"),
          ("404.md", Just "404.md")
        ]
