module Site.Spec.Util where

import Control.Exception (bracket)
import qualified Data.Set as S
import Hakyll.Core.Compiler.Internal
import Hakyll.Core.Configuration
import Hakyll.Core.Identifier
import qualified Hakyll.Core.Logger as Logger
import Hakyll.Core.Provider
import Hakyll.Core.Store (Store)
import qualified Hakyll.Core.Store as Store
import Hakyll.Core.Util.File

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
cleanTestEnv _ = do
  foldl (>>) mempty (removeDirectory . ($ testConfiguration) <$> cleanDirectories)
  where
    cleanDirectories =
      [ destinationDirectory,
        storeDirectory,
        tmpDirectory
      ]
