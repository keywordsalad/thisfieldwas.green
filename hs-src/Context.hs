module Context where

import Hakyll
import System.Process
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defaultContext

archiveCtx :: [Item String] -> Context String
archiveCtx posts =
  listField "posts" postCtx (return posts)
    <> constField "title" "Archives"
    <> defaultContext

gitTag :: String -> Context String
gitTag key = field key $ \item -> do
  let fp = "provider/" ++ (toFilePath $ itemIdentifier item)
      gitLog format =
        readProcess
          "git"
          [ "log",
            "-1",
            "HEAD",
            "--pretty=format:" ++ format,
            fp
          ]
          ""

  unsafeCompiler $ do
    sha <- gitLog "%h"
    message <- gitLog "%s"

    let history = "https://github.com/lmcgrath/logans-blog/commits/source/" ++ fp
        commit = "https://github.com/lmcgrath/logans-blog/commit/" ++ sha

    return $
      if null sha
        then "Not Committed"
        else renderHtml $ do
          H.a ! A.href (toValue history) $ "History"
          H.span ! A.class_ "hash" $ do
            toHtml (", " :: String)
            H.a ! A.href (toValue commit) ! A.title (toValue message) $ toHtml sha
