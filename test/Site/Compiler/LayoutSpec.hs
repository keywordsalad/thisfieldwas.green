module Site.Compiler.LayoutSpec where

import Site.Compiler.Layout
import Site.TestSupport

spec :: Spec
spec = do
  let siteConfig = defaultSiteConfig
  around (withDefaultTestEnv `providing` runCompilerSpec) do
    describe "applyLayoutFromMetadata" do
      let compiler = compileBody (applyLayoutFromMetadata siteConfig)
      it "applies the layout specified in the item body's metadata" \run -> do
        let identifier = fromFilePath "data/applyLayoutFromMetadata/content.md"
        _result <- run compiler identifier
        _expected <- readFile "test/data/applyLayoutFromMetadata/expected.md"
        -- result `shouldProduce` expected
        pendingWith "Figure out how to pull together layout dependency"
