module Green.Rule where

import Green.Common
import Green.Config
import Green.Content.Blog
import Green.Content.BrokenLinks
import Green.Content.Code
import Green.Content.Css
import Green.Content.Download
import Green.Content.Feed
import Green.Content.HomePage
import Green.Content.Image
import Green.Content.Js
import Green.Content.Page
import Green.Content.Robot
import Green.Content.Sitemap
import Green.Content.Template
import Green.Template.Custom

rules :: SiteConfig -> Rules ()
rules config = do
  let context = customContext config
  brokenLinks
  imageRules
  jsRules
  scssRules config
  downloadRules
  codeDep <- codeRules
  rulesExtraDependencies [codeDep] do
    templateRules
    blogRules context
    feedRules
    homePageRules context
    pageRules context
    robotsTxtRules context
    archiveRules context
    sitemapRules
    brokenLinks
