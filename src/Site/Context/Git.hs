module Site.Context.Git (gitCommitFields) where

import Site.Common
import System.Directory (doesFileExist)
import System.Exit
import System.Process

gitCommitFields :: [Context String]
gitCommitFields =
  [ field "gitSha1" gitSha1Compiler
  , field "gitMessage" gitMessageCompiler
  , field "gitCommitPartial" gitCommitPartialCompiler
  ]

itemSourcePath :: Item a -> FilePath
itemSourcePath item = toFilePath (itemIdentifier item)

gitSha1Compiler :: Item a -> Compiler String
gitSha1Compiler = gitLogField "[no-commit]" "%h"

gitMessageCompiler :: Item a -> Compiler String
gitMessageCompiler = gitLogField "[no-log]" "%s"

gitLogField :: String -> LogFormat -> Item a -> Compiler String
gitLogField default' format item =
  unsafeCompiler $ fromMaybe default' <$> gitLog format (itemSourcePath item)

gitCommitPartialCompiler :: Item a -> Compiler String
gitCommitPartialCompiler item = unsafeCompiler gitCommitPartial
  where
    gitCommitPartial = do
      generated <- isGenerated
      if generated
        then return "source-generated.html"
        else do
          changed <- gitChanged
          return $ bool "source-commit.html" "source-changed.html" changed
    isGenerated = not <$> doesFileExist filePath
    gitChanged = do
      let args = ["diff", "--cached", filePath]
      (exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
      return $ exitCode == ExitSuccess && null stdout
    filePath = itemSourcePath item

type LogFormat = String

gitLog :: LogFormat -> FilePath -> IO (Maybe String)
gitLog format filePath = do
  let args = ["log", "-1", "HEAD", "--pretty=format:" ++ format, filePath]
  (_exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
  return if null stdout then Nothing else Just stdout
