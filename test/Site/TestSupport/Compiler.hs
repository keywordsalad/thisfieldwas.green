module Site.TestSupport.Compiler where

import Data.Set as S
import Hakyll as H
import Hakyll.Core.Compiler.Internal
import qualified Hakyll.Core.Logger as Logger
import Site.TestSupport.TestEnv
import Test.Hspec

type RunCompiler a = Compiler a -> Identifier -> IO (CompilerResult a)

runCompilerSpec :: TestEnv -> (RunCompiler a -> IO b) -> IO b
runCompilerSpec testEnv f = do
  let run compiler identifier = do
        logger <- Logger.new Logger.Debug
        let compilerRead =
              CompilerRead
                { compilerConfig = testHakyllConfig testEnv,
                  compilerUnderlying = identifier,
                  compilerProvider = testProvider testEnv,
                  compilerUniverse = S.empty,
                  compilerRoutes = mempty,
                  compilerStore = testStore testEnv,
                  compilerLogger = logger
                }
        result <- runCompiler compiler compilerRead
        Logger.flush logger
        return result
  f run

shouldProduce :: (Eq a, Show a) => CompilerResult (Item a) -> a -> Expectation
shouldProduce (CompilerDone item _) expected =
  itemBody item `shouldBe` expected
shouldProduce (CompilerSnapshot snapshot _) _ =
  expectationFailure $ "Compiler did not complete, received snapshot " ++ snapshot
shouldProduce (CompilerRequire (identifier, snapshot) _) _ =
  expectationFailure $ "Compiler did not complete, produced dependency on " ++ show identifier ++ " snapshot " ++ snapshot
shouldProduce (CompilerError errors) _ = expectationFailure message
  where
    message = case errors of
      CompilationFailure exceptions -> "Compiler failed with exceptions: " ++ show exceptions
      CompilationNoResult [] -> "Compiler produced no result"
      CompilationNoResult exceptions -> "Compiler produced no result with exceptions: " ++ show exceptions

compileBody :: (Item String -> Compiler (Item String)) -> Compiler (Item String)
compileBody compiler = getUnderlying >>= loadBody >>= compiler
