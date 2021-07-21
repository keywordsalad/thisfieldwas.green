module Green.TestSupport.TestEnv where

import Data.Time
import Green.Common
import Green.Config
import Green.TestSupport.Config
import Hakyll as H
import qualified Hakyll.Core.Provider as HP
import qualified Hakyll.Core.Store as HS

data TestEnv = TestEnv
  { testTime :: LocalTime,
    testHakyllConfig :: H.Configuration,
    testStore :: HS.Store,
    testProvider :: HP.Provider,
    testSiteConfig :: SiteConfig
  }

newStoreAndProvider :: H.Configuration -> IO (HS.Store, HP.Provider)
newStoreAndProvider hakyllConfig = do
  store <- newStore
  provider <- newProvider store
  return (store, provider)
  where
    newStore = HS.new True (storeDirectory hakyllConfig)
    newProvider store = HP.newProvider store (const (return False)) (providerDirectory hakyllConfig)

createTestEnv :: H.Configuration -> IO TestEnv
createTestEnv hakyllConfig = do
  time <- defaultTestTime
  (store, provider) <- newStoreAndProvider hakyllConfig
  return
    TestEnv
      { testTime = time,
        testHakyllConfig = hakyllConfig,
        testStore = store,
        testProvider = provider,
        testSiteConfig = defaultSiteConfigWith hakyllConfig
      }

cleanTestEnv :: TestEnv -> IO ()
cleanTestEnv testEnv =
  let hakyllConfig = testHakyllConfig testEnv
   in sequenceA_ (removeDirectory . ($ hakyllConfig) <$> cleanDirectories)
  where
    cleanDirectories =
      [ destinationDirectory,
        storeDirectory,
        tmpDirectory
      ]

withTestEnv :: H.Configuration -> (TestEnv -> IO a) -> IO a
withTestEnv hakyllConfig = bracket (createTestEnv hakyllConfig) cleanTestEnv

withDefaultTestEnv :: (TestEnv -> IO a) -> IO a
withDefaultTestEnv = withTestEnv defaultHakyllConfig
