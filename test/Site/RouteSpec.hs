module Site.RouteSpec where

import Data.Bifunctor
import Site.TestSupport

spec :: Spec
spec = do
  describe "appendIndexHtml" do
    let ids =
          first fromFilePath
            <$> [ ("this/appends/it.html", "this/appends/it/index.html"),
                  ("this/does/not/append/index.html", "this/does/not/append/index.html"),
                  ("index.html", "index.html")
                ]
    runAll $ flip fmap ids \(from', to') ->
      it ("routes " ++ toFilePath from' ++ " into " ++ to') do
        appendIndexHtml from' `shouldBe` to'

  around (withDefaultTestEnv `providing` runRouteSpec) do
    describe "indexRoute" do
      runRouteExamples indexRoute $
        [ ("info/tag-cloud.html", Just "info/tag-cloud/index.html"),
          ("contact.html", Just "contact/index.html"),
          ("contact/index.html", Just "contact/index.html")
        ]
    describe "stripPrefixRoute" do
      runRouteExamples (stripPrefixRoute "^pages/") $
        [ ("pages/contact.md", Just "contact.md"),
          ("pages/archives.html", Just "archives.html"),
          ("404.md", Just "404.md")
        ]
    describe "subPrefixRoute" do
      runRouteExamples (subPrefixRoute "^pages/" "potatoes/") $
        [ ("pages/contact.md", Just "potatoes/contact.md"),
          ("pages/archives.html", Just "potatoes/archives.html"),
          ("404.md", Just "404.md")
        ]
    describe "pageRoute" do
      runRouteExamples pageRoute $
        [ ("pages/contact.md", Just "contact/index.html"),
          ("pages/archives.md", Just "archives/index.html"),
          ("pages/about-me.md", Just "about-me/index.html")
        ]
