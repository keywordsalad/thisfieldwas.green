module Green where

import qualified Data.Text.IO as TIO
import Data.Time
import Green.Command
import Green.Common
import Green.Config
import Green.Content
import qualified Hakyll as H
import Options.Applicative
import System.Environment

site :: IO ()
site = do
  siteConfig <- loadSiteConfig
  putStrLn $ replicate 80 '-'
  print siteConfig
  putStrLn $ replicate 80 '-'
  H.hakyllWith (siteConfig ^. siteHakyllConfiguration) (content siteConfig)

author :: IO ()
author = do
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
  let result = parseConfigIni env defaultTimeLocale time configIniText
  either fail return result
