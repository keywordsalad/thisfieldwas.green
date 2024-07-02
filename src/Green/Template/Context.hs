module Green.Template.Context where

import Green.Common
import Green.Config
import Green.Site.Blog (loadPublishedPosts)
import Green.Template.HtmlField
import Hakyll (recentFirst)
import Green.Hakyllbars as HB

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
          constField "currentTime" currentTime,
          constField "siteTitle" (info ^. siteTitle),
          constField "siteDescription" (info ^. siteDescription),
          constField "siteRoot" (info ^. siteRoot),
          constField "siteCommentsId" (info ^. siteCommentsId),
          constField "authorEmail" (info ^. siteAuthorEmail),
          constField "authorName" (info ^. siteAuthorName),
          constField "author" (info ^. siteAuthorName), -- default to authorName
          constField "linkedInProfile" (info ^. siteLinkedInProfile),
          constField "githubProfile" (info ^. siteGitHubProfile),
          constField "giteaProfile" (info ^. siteGiteaProfile),
          constField "useSocial" True,
          constField "article" False,
          constField "commentsSiteId" (info ^. siteCommentsId),
          dateFields dateConfig,
          gitFields (config ^. siteProviderDirectory) (info ^. siteGiteaWebUrl),
          defaultFields (info ^. siteHost) (info ^. siteRoot)
        ]
    currentTime =
      formatTime
        (config ^. siteTimeLocale)
        (config ^. siteDisplayFormat . displayRobotTime)
        (config ^. siteCurrentTime)
    info = config ^. siteInfo
    dateConfig = defaultDateConfigWith (config ^. siteTimeLocale) (config ^. siteCurrentTime)
