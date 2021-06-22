module GreenSite where

import qualified Data.Text.IO as TIO
import Data.Time
import GreenSite.Command
import GreenSite.Common
import GreenSite.Rule (rules)
import Options.Applicative (execParser)
import System.Environment (getEnvironment, getProgName)

site :: IO ()
site = do
  env <- getEnvironment
  zonedTime <- getZonedTime

  configIniText <- TIO.readFile "config.ini"
  siteConfig <- case parseConfigIni env zonedTime configIniText of
    Left e -> fail e
    Right config -> return $ config & siteContext %~ baseContext config

  hakyllWith (siteConfig ^. siteHakyllConfiguration) (rules siteConfig)

green :: IO ()
green = do
  progName <- getProgName
  processGreenCommand =<< execParser (greenCommands progName)
