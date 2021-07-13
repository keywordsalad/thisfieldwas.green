module Green.Context.GitCommits (gitCommits) where

import Data.Binary
import GHC.Generics (Generic)
import Green.Common
import Green.Config
import System.Exit
import System.Process

data SourceFile = SourceFile
  { _sourceFilePath :: String,
    _sourceFileIsFromSource :: Bool,
    _sourceFileIsChanged :: Bool
  }
  deriving stock (Generic)

makeLenses ''SourceFile

instance Binary SourceFile where
  get = SourceFile <$> get <*> get <*> get
  put sourceFile =
    put (sourceFile ^. sourceFilePath)
      >> put (sourceFile ^. sourceFileIsFromSource)
      >> put (sourceFile ^. sourceFileIsChanged)

gitCommits :: SiteConfig -> Context String
gitCommits config =
  mconcat
    [ constField "gitWebUrl" (config ^. siteGitWebUrl),
      field "gitSha1" gitSha1Compiler,
      field "gitMessage" gitMessageCompiler,
      field "gitBranch" gitBranchCompiler,
      sourceFileField (config ^. siteProviderDirectory)
    ]

itemFilePath :: Item a -> FilePath
itemFilePath = toFilePath . itemIdentifier

gitSha1Compiler :: Item a -> Compiler String
gitSha1Compiler = gitLogField "%h"

gitMessageCompiler :: Item a -> Compiler String
gitMessageCompiler = gitLogField "%s"

type LogFormat = String

gitLogField :: LogFormat -> Item a -> Compiler String
gitLogField format item =
  unsafeCompiler do
    maybeResult <- gitLog format (Just $ itemFilePath item)
    case maybeResult of
      Just result -> return result
      Nothing -> fromJust <$> gitLog format Nothing

sourceFileField :: FilePath -> Context String
sourceFileField providerDirectory = Context \k _ i ->
  if k `elem` [filePathKey, fileNameKey]
    then getField k =<< sourceFileCompiler providerDirectory i
    else unmappedKey k
  where
    getField key sourceFile
      | key == filePathKey = return $ StringField $ sourceFile ^. sourceFilePath
      | key == fileNameKey = return $ StringField $ takeFileName $ sourceFile ^. sourceFilePath
      | key == isFromSourceKey = boolField' key $ sourceFile ^. sourceFileIsFromSource
      | key == isChangedKey = boolField' key $ sourceFile ^. sourceFileIsChanged
      | otherwise = unmappedKey key
    boolField' _ True = return EmptyField
    boolField' key False = noResult $ "Field " ++ key ++ " is false"
    unmappedKey key = noResult $ "Tried sourceFileField with unmapped key " ++ key
    filePathKey = "sourceFilePath"
    fileNameKey = "sourceFileName"
    isFromSourceKey = "isFromSource"
    isChangedKey = "isChanged"

sourceFileCompiler :: FilePath -> Item a -> Compiler SourceFile
sourceFileCompiler providerDirectory item = cached cacheKey do
  SourceFile itemFilePath'
    <$> unsafeCompiler (doesFileExist itemFilePath')
    <*> unsafeCompiler (isChanged itemFilePath')
  where
    itemFilePath' = providerDirectory </> itemFilePath item
    cacheKey = toFilePath (itemIdentifier item) ++ ":sourceFileField"
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
