module Site.RouteSpec where

import Data.Bifunctor
import Hakyll.Core.Identifier
import Site.Common
import Site.Spec.Util
import Test.Hspec

spec :: Spec
spec = do
  around withRunRoutes do
    describe "indexRoute" do
      runTable indexRoute $
        [ ("info/tag-cloud.html", "info/tag-cloud/index.html"),
          ("contact.html", "contact/index.html"),
          ("contact/index.html", "contact/index.html")
        ]
    describe "stripPrefixRoute" do
      runTable (stripPrefixRoute "^pages/") $
        [ ("pages/contact.md", "contact.md"),
          ("pages/archives.html", "archives.html"),
          ("404.md", "404.md")
        ]
    describe "htmlPageRoute" do
      runTable htmlPageRoute $
        [ ("pages/contact.md", "contacts/index.html"),
          ("pages/archives.md", "archives/index.html"),
          ("pages/about-me.md", "about-me/index.html")
        ]

runTable :: Routes -> [(String, String)] -> SpecWith RunRoutes
runTable routes = foldl (>>) (return ()) . fmap makeExample
  where
    makeExample inputOutput = it ("routes " ++ fst inputOutput ++ " to " ++ snd inputOutput) $ runExample inputOutput
    runExample inputOutput runRoutes' =
      let applyRoutes = fmap fst . runRoutes' routes . fromFilePath
          (actual, expected) = first applyRoutes inputOutput
       in actual >>= (`shouldBe` Just expected)

type RunRoutes = Routes -> Identifier -> IO (Maybe FilePath, UsedMetadata)

withRunRoutes :: (RunRoutes -> IO a) -> IO a
withRunRoutes f = withStoreAndProvider \(_, provider) -> f (`runRoutes` provider)
