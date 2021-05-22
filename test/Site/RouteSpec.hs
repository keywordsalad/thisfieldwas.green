module Site.RouteSpec where

import Control.Exception (bracket)
import Data.Bifunctor
import Hakyll.Core.Identifier
import Site.Common
import Site.Spec.Util
import Test.Hspec

spec :: Spec
spec = do
  around withRunRoutes do
    describe "indexRoute" do
      it "appends the suffix /index.html if not there already" $
        runSpec indexRoute $
          [ ("info/tag-cloud.html", "info/tag-cloud/index.html"),
            ("contact.html", "contact/index.html"),
            ("contact/index.html", "contact/index.html")
          ]

    describe "stripPrefixRoute" do
      it "strips the prefix pages/ if it exists" $
        runSpec (stripPrefixRoute "^pages/") $
          [ ("pages/contact.md", "contact.md"),
            ("pages/archives.html", "archives.html"),
            ("404.md", "404.md")
          ]

    describe "htmlPageRoute" do
      it "sets the .html extension, strips the pages/ prefix, and adds /index.html suffix" $
        runSpec htmlPageRoute $
          [ ("pages/contact.md", "contacts/index.html"),
            ("pages/archives.md", "archives/index.html"),
            ("pages/about-me.md", "about-me/index.html")
          ]

type RunRoutes = Routes -> Identifier -> IO (Maybe FilePath, UsedMetadata)

runSpec :: Routes -> [(String, String)] -> RunRoutes -> Expectation
runSpec routes testInputOutput runRoutes' =
  runExpectations setExpectations
  where
    setExpectations = expect . bimap runRoutes'' Just <$> testInputOutput
    expect (actualM, expected) = actualM >>= (`shouldBe` expected)
    runRoutes'' = fmap fst . runRoutes' routes . fromFilePath

withRunRoutes :: (RunRoutes -> IO a) -> IO a
withRunRoutes = bracket acquire release
  where
    acquire = flip runRoutes . snd <$> createStoreAndProvider
    release = const (return ())

withRunRoutes' :: (RunRoutes -> IO a) -> IO a
withRunRoutes' f = withStoreAndProvider \(_, provider) -> f (`runRoutes` provider)
