module Site.Spec.Common
  ( module Hakyll,
    module Test.Hspec,
    module Site.Common,
    runAll,
    createStoreAndProvider,
    newTestProvider,
    newTestStore,
    withStoreAndProvider,
    testCompiler,
    testConfiguration,
    cleanTestEnv,
    runRoutesTable,
    withRunRoutes,
  )
where

import Control.Exception (bracket)
import qualified Data.Set as S
import Hakyll
import Hakyll.Core.Compiler.Internal
import qualified Hakyll.Core.Logger as Logger
import Hakyll.Core.Provider
import Hakyll.Core.Store (Store)
import qualified Hakyll.Core.Store as Store
import Site.Common
import Test.Hspec

runAll :: [SpecWith a] -> SpecWith a
runAll = sequenceA_

createStoreAndProvider :: IO (Store, Provider)
createStoreAndProvider = do
  store <- newTestStore
  provider <- newTestProvider store
  return (store, provider)

withStoreAndProvider :: ((Store, Provider) -> IO a) -> IO a
withStoreAndProvider = bracket createStoreAndProvider cleanTestEnv

newTestStore :: IO Store
newTestStore = Store.new True $ storeDirectory testConfiguration

newTestProvider :: Store -> IO Provider
newTestProvider store =
  newProvider store (const $ return False) $
    providerDirectory testConfiguration

testCompiler :: Store -> Provider -> Identifier -> Compiler a -> IO (CompilerResult a)
testCompiler store provider underlying compiler = do
  logger <- Logger.new Logger.Error
  let read' =
        CompilerRead
          { compilerConfig = testConfiguration,
            compilerUnderlying = underlying,
            compilerProvider = provider,
            compilerUniverse = S.empty,
            compilerRoutes = mempty,
            compilerStore = store,
            compilerLogger = logger
          }

  result <- runCompiler compiler read'
  Logger.flush logger
  return result

testConfiguration :: Configuration
testConfiguration =
  defaultConfiguration
    { destinationDirectory = "_test/site",
      storeDirectory = "_test/store",
      tmpDirectory = "_test/tmp",
      providerDirectory = "test/data"
    }

cleanTestEnv :: a -> IO ()
cleanTestEnv _ = sequenceA_ (removeDirectory . ($ testConfiguration) <$> cleanDirectories)
  where
    cleanDirectories =
      [ destinationDirectory,
        storeDirectory,
        tmpDirectory
      ]

runRoutesTable :: Routes -> [(String, Maybe String)] -> SpecWith RunRoutes
runRoutesTable routes = runAll . fmap makeExample
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

type RunRoutes = Routes -> Identifier -> IO (Maybe FilePath, UsedMetadata)

withRunRoutes :: (RunRoutes -> IO a) -> IO a
withRunRoutes f = withStoreAndProvider \(_, provider) -> f (`runRoutes` provider)
