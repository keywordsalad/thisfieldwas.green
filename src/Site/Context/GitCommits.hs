module Site.Context.GitCommits (gitCommits) where

import Data.Bool (bool)
import Data.Maybe (fromJust, isJust)
import Hakyll
import System.Directory (doesFileExist)
import System.Exit
import System.Process

gitCommits :: String -> Context String
gitCommits gitWebUrl =
  mconcat
    [ constField "git-web-url" gitWebUrl,
      field "git-sha1" gitSha1Compiler,
      field "git-message" gitMessageCompiler,
      field "is-changed" isChangedCompiler,
      field "is-generated" isGeneratedCompiler,
      field "git-branch" gitBranchCompiler
    ]

itemSourcePath :: Item a -> FilePath
itemSourcePath item = toFilePath (itemIdentifier item)

gitSha1Compiler :: Item a -> Compiler String
gitSha1Compiler = gitLogField "%h"

gitMessageCompiler :: Item a -> Compiler String
gitMessageCompiler = gitLogField "%s"

gitLogField :: LogFormat -> Item a -> Compiler String
gitLogField format item =
  unsafeCompiler do
    maybeResult <- gitLog format (Just $ itemSourcePath item)
    case maybeResult of
      Just result -> return result
      Nothing -> fromJust <$> gitLog format Nothing

isGeneratedCompiler :: Item a -> Compiler String
isGeneratedCompiler item = do
  generated <- unsafeCompiler $ isGenerated filePath
  if generated
    then return "generated"
    else noResult $ "Was not generated: " ++ filePath
  where
    filePath = itemSourcePath item

isGenerated :: FilePath -> IO Bool
isGenerated = fmap not . doesFileExist

isChangedCompiler :: Item a -> Compiler String
isChangedCompiler item = do
  changed <- unsafeCompiler do isChanged filePath
  if changed
    then return "changed"
    else noResult $ "Was not changed: " ++ filePath
  where
    filePath = itemSourcePath item

isChanged :: FilePath -> IO Bool
isChanged filePath = do
  let args = ["diff", "HEAD", filePath]
  (exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
  return $ not (exitCode == ExitSuccess && null stdout)

type LogFormat = String

gitLog :: LogFormat -> Maybe FilePath -> IO (Maybe String)
gitLog format filePath = do
  let fpArgs = bool [fromJust filePath] [] (isJust filePath)
  let args = ["log", "-1", "HEAD", "--pretty=format:" ++ format] ++ fpArgs
  (_exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
  return if null stdout then Nothing else Just stdout

gitBranchCompiler :: Item a -> Compiler String
gitBranchCompiler _ = unsafeCompiler gitBranch

gitBranch :: IO String
gitBranch = do
  let args = ["branch", "--show-current"]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "git" args ""
  if exitCode == ExitSuccess
    then return stdout
    else fail $ "Unable to get current branch: " ++ stderr
