module Green.Template.Context where

import Green.Common
import Green.Config
import Green.Hakyllbars as HB
import Green.Site.Blog (loadPublishedPosts)
import Green.Template.HtmlField
import Hakyll (recentFirst)

customContext :: SiteConfig -> Context String
customContext config = self
  where
    latestPostPatterns =
      fromFilePath
        <$> [ "blog.html",
              "archives.html",
              "categories.html",
              "tags.html",
              "drafts.html"
            ]
    latestPostUpdated (latestPost : _) = tplWithItem latestPost (unContext self "updated")
    latestPostUpdated _ = tplTried "latest post updated"
    self =
      mconcat
        [ trimmedUrlField "url",
          forItemField "updated" latestPostPatterns \_ -> do
            latestPosts <- lift $ recentFirst =<< loadPublishedPosts
            latestPostUpdated latestPosts,
          escapeHtmlField,
          escapeHtmlUriField,
          imageFigureField,
          youtubeField,
          codeField,
          inlineStylesheetField "inlineStylesheet",
          constField "currentTime" currentTime,
          constField "siteTitle" (info ^. siteTitle),
          constField "siteDescription" (info ^. siteDescription),
          constField "siteRoot" (info ^. siteRoot),
          constField "authorEmail" (info ^. siteAuthorEmail),
          constField "authorName" (info ^. siteAuthorName),
          constField "author" (info ^. siteAuthorName), -- default to authorName
          constField "linkedInProfile" (info ^. siteLinkedInProfile),
          constField "githubProfile" (info ^. siteGitHubProfile),
          constField "mastodonProfile" (info ^. siteMastodonProfile),
          constField "useSocial" True,
          constField "article" False,
          dateFields dateConfig,
          gitFields (config ^. siteProviderDirectory) (info ^. siteGitHubWebUrl),
          defaultFields (info ^. siteHost) (info ^. siteRoot)
        ]
    currentTime =
      formatTime
        (config ^. siteTimeLocale)
        (config ^. siteDisplayFormat . displayRobotTime)
        (config ^. siteCurrentTime)
    info = config ^. siteInfo
    dateConfig = defaultDateConfigWith (config ^. siteTimeLocale) (config ^. siteCurrentTime)

-- | Loads the compiled body of a stylesheet (by its source identifier, e.g.
-- @css/pages/homepage.scss@) so it can be dropped into a @<style>@ tag and
-- inlined into the page, avoiding an extra render-blocking request. Loading
-- through Hakyll keeps the dependency tracked.
inlineStylesheetField :: String -> Context String
inlineStylesheetField key = functionField key f
  where
    f (filePath :: FilePath) =
      dropCharset <$> lift (loadBody $ fromFilePath filePath :: Compiler String)
    -- `@charset` is only valid at the very start of an external stylesheet; inside
    -- a `<style>` tag it is an ignored no-op, so drop the one Dart Sass emits (the
    -- document's UTF-8 encoding governs the inlined content).
    dropCharset css
      | "@charset " `isPrefixOf` css = drop 1 (dropWhile (/= ';') css)
      | otherwise = css
