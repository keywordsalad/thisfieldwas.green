module GreenSite.TestSupport.Routes where

import Data.Foldable (traverse_)
import Hakyll as H
import GreenSite.Common
import GreenSite.TestSupport.TestEnv
import Test.Hspec

type RunRoutes = Routes -> Identifier -> IO (Maybe FilePath, UsedMetadata)

runRouteSpec :: TestEnv -> (RunRoutes -> IO a) -> IO a
runRouteSpec testEnv f = f (`H.runRoutes` testProvider testEnv)

runRouteExamples :: Routes -> [(String, Maybe String)] -> SpecWith RunRoutes
runRouteExamples routes = traverse_ makeExample
  where
    makeExample inputOutput = it (makeLabel inputOutput) $ runExample inputOutput
    makeLabel (input, Just output)
      | input == output = "does not change the route of " ++ input
      | otherwise = "routes " ++ input ++ " to " ++ output
    makeLabel (input, Nothing) = "does not route " ++ input
    runExample inputOutput runRoutes' =
      let applyRoutes = fmap fst . runRoutes' routes . fromFilePath
          (actual, expected) = first applyRoutes inputOutput
       in actual >>= (`shouldBe` expected)
