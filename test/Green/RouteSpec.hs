module Green.RouteSpec where

import Data.Bifunctor
import Green.TestSupport

spec :: Spec
spec = do
  describe "appendIndexHtml" do
  around (withDefaultTestEnv `providing` runRouteSpec) do
    describe "intoIndex" do
      runRouteExamples intoIndex $
        [ ("info/tag-cloud.html", Just "info/tag-cloud/index.html"),
          ("contact.html", Just "contact/index.html"),
          ("contact/index.html", Just "contact/index.html")
        ]
    describe "stripPrefixRoute" do
      runRouteExamples (stripPrefixRoute "pages/") $
        [ ("pages/contact.md", Just "contact.md"),
          ("pages/archives.html", Just "archives.html"),
          ("404.md", Just "404.md")
        ]
    describe "subPrefixRoute" do
      runRouteExamples (subPrefixRoute "pages/" "potatoes/") $
        [ ("pages/contact.md", Just "potatoes/contact.md"),
          ("pages/archives.html", Just "potatoes/archives.html"),
          ("404.md", Just "404.md")
        ]
    describe "subRoute" do
      runRouteExamples (subRoute "/(cats|birds)/" "/dogs/") $
        [ ("pages/cats/contact.md", Just "pages/dogs/contact.md"),
          ("pages/birds/archives.html", Just "pages/dogs/archives.html"),
          ("pages/404.md", Just "pages/404.md")
        ]
    describe "pageRoute" do
      runRouteExamples pageRoute $
        [ ("pages/contact.md", Just "contact/index.html"),
          ("pages/archives.md", Just "archives/index.html"),
          ("pages/about-me.md", Just "about-me/index.html")
        ]
