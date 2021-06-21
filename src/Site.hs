module Site where

import qualified Data.Text.IO as TIO
import Data.Time
import Site.Common
import Site.Rule (rules)
import System.Environment (getEnvironment)

site :: IO ()
site = do
  env <- getEnvironment
  zonedTime <- getZonedTime

  configIniText <- TIO.readFile "config.ini"
  siteConfig <- case parseConfigIni env zonedTime configIniText of
    Left e -> fail e
    Right config -> return $ config & siteContext %~ baseContext config

  hakyllWith (siteConfig ^. siteHakyllConfiguration) (rules siteConfig)
