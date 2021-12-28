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
        [ forItemField "updated" latestPostPatterns \_ -> do
            latestPosts <- lift $ recentFirst =<< loadPublishedPosts
            latestPostUpdated latestPosts,
          trimmedUrlField "url",
          includeField "include" "",
          includeField "partial" "_partials",
          layoutField "applyLayout" "_layouts",
          dateFields config,
          gitCommits config,
          constField "siteTitle" (config ^. siteInfo . siteTitle),
          constField "siteRoot" (config ^. siteInfo . siteRoot),
          constField "currentTime" (formatTime timeLocale robotTime currentTime),
          constField "linkedInProfile" (config ^. siteInfo . siteLinkedInProfile),
          constField "authorEmail" (config ^. siteInfo . siteAuthorEmail),
          escapeHtmlField,
          escapeHtmlUriField,
          imgField,
          youtubeField,
          codeField,
          defaultFields
        ]
    timeLocale = config ^. siteTimeLocale
    robotTime = config ^. siteDisplayFormat . displayRobotTime
    currentTime = config ^. siteCurrentTime
