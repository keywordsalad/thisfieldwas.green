module Site where

import Hakyll
import Site.Configuration
import Site.Context.Git
import Site.Rule
import Site.Util
import System.Environment (getEnvironment)

site :: IO ()
site = do
  env <- getEnvironment
  absRoot <- loadAbsRoot
  hakyllWith hakyllConfiguration do
    tags <- buildTags "blog/*" $ fromCapture "tags/*.html"
    let baseCtx =
          constField "absRoot" absRoot
          <> tagsField "tags" tags
          <> cleanIndexPaths "url"
          <> mconcat gitCommitFields
          <> defaultContext

    rules env (feedConfig absRoot) baseCtx
