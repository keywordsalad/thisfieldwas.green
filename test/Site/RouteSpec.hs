module Site.RouteSpec where

import Data.Bifunctor
import Hakyll.Core.Identifier
import Site.Common
import Site.SpecUtil
import Test.Hspec

spec :: Spec
spec = do
  around withRunRoutes do
    describe "indexRoute" do
      let inputsOutputs =
            bimap fromFilePath toFilePath
              <$> [ ("info/tag-cloud.html", "info/tag-cloud/index.html"),
                    ("contact.html", "contact/index.html"),
                    ("contact/index.html", "contact/index.html")
                  ]
      it "appends an index.html to the identifier path" \run -> do
        runIOs $
          inputsOutputs <&> \(input, output) -> do
            (`shouldBe` Just output) . fst =<< run indexRoute input

    describe "stripPrefixRoute" do
      let inputsOutputs =
            first fromFilePath
              <$> [ ("pages/contact.md", "contact.md"),
                    ("pages/archives.html", "archives.html"),
                    ("404.md", "404.md")
                  ]
      it "strips the prefix from the identifier path" \run -> do
        runIOs $
          inputsOutputs <&> \(input, output) -> do
            (`shouldBe` Just output) . fst =<< run indexRoute input
