module Site.RouteSpec where

import Data.Bifunctor
import Site.Spec.Common

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
      it ("changes " ++ toFilePath from' ++ " into " ++ to') do
        appendIndexHtml from' `shouldBe` to'

  around withRunRoutes do
    describe "pageRoute" do
      runRoutesTable pageRoute $
        [ ("pages/hello.txt", Just "hello.txt"),
          ("pages/contact.html", Just "contact.html"),
          ("pages/special.pdf", Just "special.pdf")
        ]

    describe "indexRoute" do
      runRoutesTable indexRoute $
        [ ("info/tag-cloud.html", Just "info/tag-cloud/index.html"),
          ("contact.html", Just "contact/index.html"),
          ("contact/index.html", Just "contact/index.html")
        ]
    describe "stripPrefixRoute" do
      runRoutesTable (stripPrefixRoute "^pages/") $
        [ ("pages/contact.md", Just "contact.md"),
          ("pages/archives.html", Just "archives.html"),
          ("404.md", Just "404.md")
        ]
    describe "htmlPageRoute" do
      runRoutesTable htmlPageRoute $
        [ ("pages/contact.md", Just "contact/index.html"),
          ("pages/archives.md", Just "archives/index.html"),
          ("pages/about-me.md", Just "about-me/index.html")
        ]
