module Green.Content (content) where

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

content :: SiteConfig -> Rules ()
content config = do
  let context = customContext config
  brokenLinks
  images
  js
  scss config
  downloads
  codeDep <- code
  rulesExtraDependencies [codeDep] do
    templates
    blog context
    feed
    homePage context
    pages context
    robotsTxt context
    sitemap context
    brokenLinks
