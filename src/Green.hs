module Green where

import qualified Data.Text.IO as TIO
import Data.Time
import Green.Command
import Green.Common
import Green.Rule
import Options.Applicative
import System.Environment

site :: IO ()
site = do
  env <- getEnvironment
  zonedTime <- getZonedTime

  configIniText <- TIO.readFile "config.ini"
  siteConfig <- case parseConfigIni env zonedTime configIniText of
    Left e -> fail e
    Right config -> return $ config & siteContext %~ baseContext config

  hakyllWith (siteConfig ^. siteHakyllConfiguration) (rules siteConfig)

author :: IO ()
author = do
  progName <- getProgName
  processAuthorCommand =<< customExecParser prefs' (authorCommands progName)
  where
    prefs' = prefs (showHelpOnError <> showHelpOnEmpty)
