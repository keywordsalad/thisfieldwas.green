module Green where

import qualified Data.Text.IO as TIO
import Data.Time
import Green.Command
import Green.Common
import Green.Config
import Green.Context
import Green.Rule
import Options.Applicative
import System.Environment

site :: IO ()
site = do
  siteConfig <- loadSiteConfig
  hakyllWith (siteConfig ^. siteHakyllConfiguration) (rules siteConfig)

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
  time <- utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
  configIniText <- TIO.readFile "config.ini"
  case parseConfigIni env defaultTimeLocale time configIniText of
    Left e -> fail e
    Right config -> return $ config & siteContext .~ baseContext config
