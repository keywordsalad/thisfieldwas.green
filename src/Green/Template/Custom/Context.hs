module Green.Template.Custom.Context where

import Green.Common
import Green.Config
import Green.Content.Blog (loadPublishedPosts)
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
            latestPosts <- lift $ recentFirst =<< loadPublishedPosts config
            latestPostUpdated latestPosts,
          trimmedUrlField "url",
          includeField "include" "",
          includeField "partial" "_partials",
          layoutField "applyLayout" "_layouts",
          dateFields config,
          gitCommits config,
          constField "siteTitle" (config ^. siteTitle),
          constField "siteRoot" (config ^. siteRoot),
          constField "linkedInProfile" (config ^. siteLinkedInProfile),
          constField "authorEmail" (config ^. siteAuthorEmail),
          imgField,
          youtubeField,
          codeField,
          defaultFields
        ]
