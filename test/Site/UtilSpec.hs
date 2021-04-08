module Site.UtilSpec (spec) where

import Site.Util
import Test.Hspec

spec :: Spec
spec = do
  describe "stripSuffix" do
    let suffix = "index.html"
    context "the suffix exists" do
      let input = "this/path/to/index.html"
      it "strips the suffix" do
        stripSuffix suffix input `shouldBe` "this/path/to/"
    context "the suffix does not exist" do
      let input = "this/path/to/something-else.html"
      it "returns the original string" do
        stripSuffix suffix input `shouldBe` input

  describe "loadAbsRoot" do
    it "creats a URL prefix with the CNAME file contents" do
      root <- loadAbsRoot
      root `shouldBe` "https://www.thisfieldwas.green"
