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
          constField "absRoot" absRoot
            <> tagsField "tags" tags
            <> cleanIndexPaths "url"
            <> mconcat gitCommitFields
            <> imgField
            <> getCodeField
            <> youtubeField
            <> getRouteField
            <> commentField
            <> defaultContext

    rules env (feedConfig absRoot) baseCtx
