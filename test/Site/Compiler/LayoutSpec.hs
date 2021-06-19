module Site.Compiler.LayoutSpec where

import Site.Compiler.Layout
import Site.TestSupport

spec :: Spec
spec = do
  let siteConfig = defaultSiteConfig
  around (withDefaultTestEnv `providing` runCompilerSpec) do
    describe "applyLayoutFromMetadata" do
      let compiler = applyLayoutFromMetadata siteConfig <=< loadBody
      it "applies the layout specified in the item body's metadata" \run -> do
        let identifier = fromFilePath "applyLayoutFromMetadata/content.md"
        result <- run compiler identifier
        expected <- readFile "test/data/applyLayoutFomeMetadata/"
        result `shouldProduce` expected
