module Green.Site where

import Green.Common
import Green.Config
import Green.Site.Blog
import Green.Site.BrokenLinks
import Green.Site.Code
import Green.Site.Css
import Green.Site.Download
import Green.Site.Feed
import Green.Site.HomePage
import Green.Site.Image
import Green.Site.Js
import Green.Site.Page
import Green.Site.Robots
import Green.Site.Sitemap
import Green.Site.Templates
import Green.Template.Custom.Context

site :: SiteConfig -> Rules ()
site config = do
  let context = customContext config
  brokenLinks
  images
  js
  scss config
  downloads
  _codeDep <- code
  _templateDep <- templates
  blog context
  feed
  homePage context
  pages context
  robotsTxt context
  sitemap context
  brokenLinks
