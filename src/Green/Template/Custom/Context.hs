module Green.Template.Custom.Context where

import Green.Config
import Green.Template.Context
import Green.Template.Custom.DateFields
import Green.Template.Custom.GitFields
import Green.Template.Custom.HtmlFields
import Green.Template.Fields
import Lens.Micro

customContext :: SiteConfig -> Context String
customContext config =
  mconcat
    [ trimmedUrlField "url",
      includeField "include" "",
      includeField "partial" "_partials",
      layoutField "layout" "_layouts",
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
