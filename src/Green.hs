module Green where

import qualified Data.Text.IO as TIO
import Data.Time
import Green.Command
import Green.Common
import Green.Config
import Green.Site
import qualified Hakyll as H
import Options.Applicative
import System.Environment

siteMain :: IO ()
siteMain = do
  siteConfig <- loadSiteConfig
  H.hakyllWith (siteConfig ^. siteHakyllConfiguration) (site siteConfig)

authorMain :: IO ()
authorMain = do
  progName <- getProgName
  siteConfig <- loadSiteConfig
  processAuthorCommand siteConfig =<< customExecParser prefs' (authorCommands progName)
  where
    prefs' = prefs (showHelpOnError <> showHelpOnEmpty)

loadSiteConfig :: IO SiteConfig
loadSiteConfig = do
  env <- getEnvironment
  time <- utcToZonedTime <$> getCurrentTimeZone <*> getCurrentTime
  configIniText <- TIO.readFile "config.ini"
  siteConfig <-
    either fail return $
      parseConfigIni env defaultTimeLocale time configIniText

  putStrLn $ replicate 80 '-'
  print siteConfig
  putStrLn $ replicate 80 '-'

  return siteConfig
