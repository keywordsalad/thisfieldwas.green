module Green.Context
  ( module Green.Context.Field,
    module Green.Context.GitCommits,
    module Green.Context.Post,
    module Green.Context.Tag,
    baseContext,
  )
where

import Green.Config
import Green.Context.Field
import Green.Context.GitCommits
import Green.Context.Post
import Green.Context.Tag
import Hakyll
import Lens.Micro

baseContext :: SiteConfig -> Context String
baseContext config =
  mconcat
    [ constField "siteRoot" (config ^. siteRoot),
      constField "linkedinProfile" (config ^. siteLinkedInProfile),
      removeIndexUrlField "url",
      gitCommits (config ^. siteGitWebUrl),
      linkedTitleField,
      imgField,
      getCodeField,
      youtubeField,
      getRouteField,
      commentField,
      siteRootField config,
      defaultContext,
      constField "body-class" "default",
      constField "contactEmail" (config ^. siteAuthorEmail)
    ]
