module Site (site) where

import qualified Data.Text as T
import Site.Common
import Site.Rule
import System.Environment (getEnvironment)

site :: IO ()
site = do
  env <- getEnvironment
  configIniText <- T.pack <$> readFile "config.ini"
  siteConfig <- case parseConfigIni env configIniText of
    Left e -> fail e
    Right config -> return $ config & siteContext %~ initContext config
  hakyllWith (siteConfig ^. siteHakyllConfiguration) (rules siteConfig)

initContext :: SiteConfig -> Context String -> Context String
initContext config context =
  constField "body-class" "default"
    <> constField "site-root" (config ^. siteRoot)
    <> cleanIndexPaths "url"
    <> gitCommits (config ^. siteGitWebUrl)
    <> imgField
    <> includeCodeField
    <> youtubeField
    <> routeToField
    <> commentField
    <> siteRootField config
    <> demoteHeadersByField
    <> context
