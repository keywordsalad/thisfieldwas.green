module Green.Template.Custom.GitField (gitCommits) where

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
    [ constField "giteaWebUrl" (config ^. siteInfo . siteGiteaWebUrl),
      field "gitSha1" (gitSha1Compiler root),
      field "gitMessage" (gitMessageCompiler root),
      field "gitBranch" gitBranchCompiler,
      gitFileField root "gitFilePath" gitFilePath,
      gitFileField root "gitFileName" (takeFileName . gitFilePath),
      gitFileField root "isFromSource" gitFileIsFromSource,
      gitFileField root "isChanged" gitFileIsChanged
    ]
  where
    root = config ^. siteProviderDirectory

gitSha1Compiler :: String -> Item a -> TemplateRunner a String
gitSha1Compiler = gitLogField "%h"

gitMessageCompiler :: String -> Item a -> TemplateRunner a String
gitMessageCompiler = gitLogField "%s"

type LogFormat = String

gitLogField :: LogFormat -> String -> Item a -> TemplateRunner a String
gitLogField format root item =
  lift $ unsafeCompiler do
    maybeResult <- gitLog format (Just $ root </> toFilePath (itemIdentifier item))
    case maybeResult of
      Just result -> return result
      Nothing -> fromJust <$> gitLog format Nothing

gitFileField :: (IntoValue v a) => String -> String -> (GitFile -> v) -> Context a
gitFileField root key f = field key $ fmap f . gitFileCompiler root

gitFileCompiler :: String -> Item a -> TemplateRunner a GitFile
gitFileCompiler root item =
  lift $
    GitFile gitFilePath
      <$> unsafeCompiler (doesFileExist gitFilePath)
      <*> unsafeCompiler (isChanged gitFilePath)
  where
    gitFilePath = root </> toFilePath (itemIdentifier item)
    isChanged filePath = do
      let args = ["diff", "HEAD", filePath]
      (exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
      return $ not (exitCode == ExitSuccess && null stdout)

gitLog :: LogFormat -> Maybe FilePath -> IO (Maybe String)
gitLog format filePath = do
  let fpArgs = bool [] [fromJust filePath] (isJust filePath)
  let args = ["log", "-1", "HEAD", "--pretty=format:" ++ format] ++ fpArgs
  (_exitCode, stdout, _stderr) <- readProcessWithExitCode "git" args ""
  return if null stdout then Nothing else Just stdout

gitBranchCompiler :: Item a -> TemplateRunner a String
gitBranchCompiler _ = lift $ unsafeCompiler gitBranch

gitBranch :: IO String
gitBranch = do
  let args = ["branch", "--show-current"]
  (exitCode, stdout, stderr) <- readProcessWithExitCode "git" args ""
  if exitCode == ExitSuccess
    then return stdout
    else fail $ "Unable to get current branch: " ++ stderr
