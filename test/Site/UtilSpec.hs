module Site.UtilSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Hello" do
    it "prints World" do
      putStrLn "Hello World"
