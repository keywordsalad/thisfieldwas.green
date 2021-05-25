module Site.Context
  ( module Site.Context.Field,
    module Site.Context.GitCommits,
    module Site.Context.Post,
    module Site.Context.Tag,
    baseContext,
  )
where

import Hakyll (Context, constField)
import Lens.Micro
import Site.Config
import Site.Context.Field
import Site.Context.GitCommits
import Site.Context.Post
import Site.Context.Tag

baseContext :: SiteConfig -> Context String -> Context String
baseContext config defaultContext =
  constField "body-class" "default"
    <> constField "site-root" (config ^. siteRoot)
    <> constField "linkedin-profile" (config ^. siteLinkedInProfile)
    <> removeIndexUrlField "url"
    <> gitCommits (config ^. siteGitWebUrl)
    <> imgField
    <> includeCodeField
    <> youtubeField
    <> routeToField
    <> commentField
    <> siteRootField config
    <> demoteHeadersByField
    <> defaultContext
