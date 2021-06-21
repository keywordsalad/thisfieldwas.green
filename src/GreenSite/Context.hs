module GreenSite.Context
  ( module GreenSite.Context.Field,
    module GreenSite.Context.GitCommits,
    module GreenSite.Context.Post,
    module GreenSite.Context.Tag,
    baseContext,
  )
where

import GreenSite.Config
import GreenSite.Context.Field
import GreenSite.Context.GitCommits
import GreenSite.Context.Post
import GreenSite.Context.Tag
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
