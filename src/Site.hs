module Site where

import Hakyll
import Site.Configuration
import Site.Context.Field
import Site.Context.Git
import Site.Rule
import Site.Util
import System.Environment (getEnvironment)

site :: IO ()
site = do
  env <- getEnvironment
  hakyllWith hakyllConfiguration do
    tags <- buildTags "blog/*" $ fromCapture "tags/*.html"
    let baseCtx =
          constField "bodyClass" "default"
            <> tagsField "tags" tags
            <> cleanIndexPaths "url"
            <> mconcat gitCommitFields
            <> imgField
            <> includeCodeField
            <> youtubeField
            <> routeToField
            <> commentField
            <> siteRootField
            <> defaultContext

    rules env baseCtx
