module Site.Compiler.LayoutSpec where

import Hakyll
import Site.Compiler.Layout
import Site.Spec.Common
import Test.Hspec

spec :: Spec
spec = do
  let siteConfig = defaultSiteConfig
  around (withDefaultTestEnv `providing` runCompilerSpec) do
    describe "applyLayoutFromMetadata" do
      let compiler = applyLayoutFromMetadata siteConfig
      it "applies the layout specified in the item body's metadata" \runCompiler' -> do
        let identifier = fromFilePath "applyLayoutFromMetadata/content.md"
        runCompiler' identifier (compiler =<< loadBody identifier)
        undefined
