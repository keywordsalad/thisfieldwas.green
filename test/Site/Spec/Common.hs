module Site.Spec.Common
  ( module H,
    module Test.Hspec,
    module Site.Common,
    module Site.Spec.Common,
  )
where

import Control.Exception (bracket)
import Control.Monad.Except
import Data.Foldable (traverse_)
import qualified Data.Set as S
import Data.Time
import Hakyll as H
import Hakyll.Core.Compiler.Internal
import qualified Hakyll.Core.Logger as Logger
import qualified Hakyll.Core.Provider as HP
import qualified Hakyll.Core.Store as HS
import Site.Common
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

data TestEnv = TestEnv
  { testTime :: ZonedTime,
    testHakyllConfig :: H.Configuration,
    testStore :: HS.Store,
    testProvider :: HP.Provider,
    testSiteConfig :: SiteConfig
  }

runAll :: [SpecWith a] -> SpecWith a
runAll = sequenceA_

defaultTestTimeString :: String
defaultTestTimeString = "2013-06-16T21:12:00-07:00"

defaultTestTime :: (MonadFail m) => m ZonedTime
defaultTestTime = timeFromString defaultTestTimeString

timeFromString :: (MonadFail m) => String -> m ZonedTime
timeFromString = parseTimeM True defaultTimeLocale "%FT%T%EZ"

withTestEnv :: H.Configuration -> (TestEnv -> IO a) -> IO a
withTestEnv hakyllConfig = bracket (createTestEnv hakyllConfig) cleanTestEnv

withDefaultTestEnv :: (TestEnv -> IO a) -> IO a
withDefaultTestEnv = withTestEnv defaultHakyllConfig

defaultFeedConfig :: H.FeedConfiguration
defaultFeedConfig =
  H.FeedConfiguration
    { feedTitle = "This Old Feed",
      feedDescription = "Feeding the old worm the good stuff",
      feedAuthorName = "Slurms McKenzie",
      feedAuthorEmail = "slurms@thisold.blog",
      feedRoot = "https://thisold.blog"
    }

defaultSiteConfig :: SiteConfig
defaultSiteConfig = defaultSiteConfigWith defaultHakyllConfig

defaultSiteConfigWith :: H.Configuration -> SiteConfig
defaultSiteConfigWith hakyllConfig =
  SiteConfig
    { _siteEnv = [],
      _siteRoot = "/",
      _siteTitle = "This Old Blog",
      _siteAuthorName = "Slurms McKenzie",
      _siteAuthorEmail = "slurms@thisold.blog",
      _siteLinkedInProfile = "https://linkedin.com/in/the-secret-ingredient",
      _siteGitWebUrl = "https://bitsof.thisold.blog/slurms/blog",
      _sitePreview = False,
      _siteDebug = False,
      _siteHakyllConfiguration = hakyllConfig,
      _siteFeedConfiguration = defaultFeedConfig,
      _siteTime = unsafePerformIO defaultTestTime,
      _siteContext = mempty
    }

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

newStoreAndProvider :: H.Configuration -> IO (HS.Store, HP.Provider)
newStoreAndProvider hakyllConfig = do
  store <- newStore
  provider <- newProvider store
  return (store, provider)
  where
    newStore = HS.new True (storeDirectory hakyllConfig)
    newProvider store = HP.newProvider store (const (return False)) (providerDirectory hakyllConfig)

defaultHakyllConfig :: H.Configuration
defaultHakyllConfig =
  defaultConfiguration
    { destinationDirectory = "_test/site",
      storeDirectory = "_test/store",
      tmpDirectory = "_test/tmp",
      providerDirectory = "test/data"
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

type RunRoutes = Routes -> Identifier -> IO (Maybe FilePath, UsedMetadata)

runRouteSpec :: TestEnv -> (RunRoutes -> IO a) -> IO a
runRouteSpec testEnv f = f (`H.runRoutes` testProvider testEnv)

type RunCompiler a = Identifier -> Compiler a -> IO (CompilerResult a)

runCompilerSpec :: TestEnv -> (RunCompiler a -> IO b) -> IO b
runCompilerSpec testEnv f = do
  let runner underlying compiler = do
        logger <- Logger.new Logger.Debug
        let compilerRead =
              CompilerRead
                { compilerConfig = testHakyllConfig testEnv,
                  compilerUnderlying = underlying,
                  compilerProvider = testProvider testEnv,
                  compilerUniverse = S.empty,
                  compilerRoutes = mempty,
                  compilerStore = testStore testEnv,
                  compilerLogger = logger
                }
        result <- runCompiler compiler compilerRead
        Logger.flush logger
        return result
  f runner

-- | Creates a spec resource dependent on a bracketed resource and runs the spec with it
--
-- Example Usage:
--     around (withBracketedResource `providing` thisComponent) do
--       describe "this thing" do
--         ...
providing ::
  -- | The factory providing the bracketed resource
  ((a -> IO c) -> IO c) ->
  -- | The factory providing the spec resource dependent on the bracketed resource
  (a -> (b -> IO c) -> IO c) ->
  -- | The spec requiring the dependent resource
  (b -> IO c) ->
  IO c
providing withResource thingForSpec theSpec = withResource (`thingForSpec` theSpec)
