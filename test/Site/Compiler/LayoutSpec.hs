module Site.Compiler.LayoutSpec where

import Site.Compiler.Layout
import Site.TestSupport

spec :: Spec
spec = do
  let siteConfig = defaultSiteConfig
  around (withDefaultTestEnv `providing` runCompilerSpec) do
    describe "applyLayoutFromMetadata" do
      let _compiler = applyLayoutFromMetadata siteConfig
      it "applies the layout specified in the item body's metadata" \_runCompiler' -> do
        let expectation :: Expectation = 1 `shouldBe` (2 :: Int)
         in expectation

-- let identifier = fromFilePath "applyLayoutFromMetadata/content.md"
-- result <- runCompiler' identifier (compiler =<< loadBody identifier)
-- expected <- readFile "test/data/applyLayoutFomeMetadata/"
-- return $ result `shouldProduce` expected
