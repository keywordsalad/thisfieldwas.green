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
