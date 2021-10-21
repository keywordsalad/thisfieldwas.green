module Green.Template.Custom.Context where

import Green.Config
import Green.Template
import Green.Template.Custom.DateField
import Green.Template.Custom.GitField
import Green.Template.Custom.HtmlField
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
