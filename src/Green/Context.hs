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
import Hakyll (Context, constField)
import Lens.Micro

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
