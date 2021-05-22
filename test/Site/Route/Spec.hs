module Site.Route.Spec where

import Hakyll.Core.Identifier
import Site.Common
import Site.Spec.Util
import Test.Hspec

runSpec :: Routes -> [(String, String)] -> RunRoutes -> Expectation
runSpec routes inputsOutputs runRoutes' = const undefined (runExpectations $ fmap runPair inputsOutputs)
  where
    runPair (input, output) = assertRun output . fst =<< runRoutes' routes (fromFilePath input)
    assertRun expected maybeActual = maybeActual `shouldBe` Just expected

spec :: Spec
spec = do
  around withRunRoutes do
    describe "indexRoute" do
      it "appends an index.html to the identifier path" $
        runSpec indexRoute $
          [ ("info/tag-cloud.html", "info/tag-cloud/index.html"),
            ("contact.html", "contact/index.html"),
            ("contact/index.html", "contact/index.html")
          ]

    describe "stripPrefixRoute" do
      it "strips the prefix from the identifier path" $
        runSpec (stripPrefixRoute "^pages/") $
          [ ("pages/contact.md", "contact.md"),
            ("pages/archives.html", "archives.html"),
            ("404.md", "404.md")
          ]

    describe "htmlPageRoute" do
      it "strips the prefix and changes creates an index html page" $
        runSpec htmlPageRoute $
          [ ("pages/contact.md", "contacts/index.html"),
            ("pages/archives.md", "archives/index.html"),
            ("pages/about-me.md", "about-me/index.html")
          ]
