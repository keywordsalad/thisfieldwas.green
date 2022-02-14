module Green.Template.Custom.Context where

import Green.Common
import Green.Config
import Green.Site.Blog (loadPublishedPosts)
import Green.Template
import Green.Template.Custom.DateField
import Green.Template.Custom.GitField
import Green.Template.Custom.HtmlField
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
            latestPosts <- lift $ recentFirst =<< loadPublishedPosts (config ^. sitePreview)
            latestPostUpdated latestPosts,
          includeField "include" Nothing,
          includeField "partial" (Just "_partials"),
          layoutField "applyLayout" "_layouts",
          dateFields config,
          gitCommits config,
          escapeHtmlField,
          escapeHtmlUriField,
          imgField,
          youtubeField,
          codeField,
          defaultFields,
          constField "currentTime" currentTime,
          constField "siteTitle" (info ^. siteTitle),
          constField "siteDescription" (info ^. siteDescription),
          constField "siteRoot" (info ^. siteRoot),
          constField "authorEmail" (info ^. siteAuthorEmail),
          constField "authorName" (info ^. siteAuthorName),
          constField "author" (info ^. siteAuthorName), -- default to authorName
          constField "linkedInProfile" (info ^. siteLinkedInProfile),
          constField "twitterProfile" (info ^. siteTwitterProfile),
          constField "twitterHandle" (info ^. siteTwitterHandle),
          constField "githubProfile" (info ^. siteGitHubProfile),
          constField "giteaProfile" (info ^. siteGiteaProfile),
          constField "useSocial" True,
          constField "article" False
        ]
    currentTime =
      formatTime
        (config ^. siteTimeLocale)
        (config ^. siteDisplayFormat . displayRobotTime)
        (config ^. siteCurrentTime)
    info = config ^. siteInfo
