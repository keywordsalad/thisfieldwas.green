module Green.Context.GitCommits (gitCommits) where

import Data.Binary
import GHC.Generics (Generic)
import Green.Common
import Green.Config
import Green.Template.Context
import System.Exit
import System.Process

data GitFile = GitFile
  { gitFilePath :: String,
    gitFileIsFromSource :: Bool,
    gitFileIsChanged :: Bool
  }
  deriving stock (Generic)

instance Binary GitFile where
  get = GitFile <$> get <*> get <*> get
  put (GitFile x y z) = put x >> put y >> put z

gitCommits :: SiteConfig -> Context a
gitCommits config =
  mconcat
    [ constField "gitWebUrl" (config ^. siteGitWebUrl),
      field "gitSha1" gitSha1Compiler,
      field "gitMessage" gitMessageCompiler,
      field "gitBranch" gitBranchCompiler,
      gitFileField "gitFilePath" gitFilePath,
      gitFileField "gitFileName" (takeFileName . gitFilePath),
      gitFileField "isFromSource" gitFileIsFromSource,
      gitFileField "isChanged" gitFileIsChanged
    ]

gitSha1Compiler :: Item a -> Compiler String
gitSha1Compiler = gitLogField "%h"

gitMessageCompiler :: Item a -> Compiler String
gitMessageCompiler = gitLogField "%s"

type LogFormat = String

gitLogField :: LogFormat -> Item a -> Compiler String
gitLogField format item =
  cached ("Green.Context.GitCommits.gitLogField:" ++ format) do
    unsafeCompiler do
      maybeResult <- gitLog format (Just $ toFilePath (itemIdentifier item))
      case maybeResult of
        Just result -> return result
        Nothing -> fromJust <$> gitLog format Nothing

gitFileField :: (IntoValue v a) => String -> (GitFile -> v) -> Context a
gitFileField key f = field key $ fmap f . gitFileCompiler

gitFileCompiler :: Item a -> Compiler GitFile
gitFileCompiler item = cached "Green.Context.GitCommits.gitFileCompiler" do
  GitFile itemFilePath
    <$> unsafeCompiler (doesFileExist itemFilePath)
    <*> unsafeCompiler (isChanged itemFilePath)
  where
    itemFilePath = toFilePath (itemIdentifier item)
    isChanged filePath = do
      let args = ["diff", "HEAD", filePath]
      (exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
      return $ not (exitCode == ExitSuccess && null stdout)

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
