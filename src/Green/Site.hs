module Green.Site where

import Green.Common
import Green.Config
import Green.Site.Blog
import Green.Site.BrokenLinks
import Green.Site.Code
import Green.Site.Css
import Green.Site.Feed
import Green.Site.HomePage
import Green.Site.Images
import Green.Site.Js
import Green.Site.Pages
import Green.Site.Robots
import Green.Site.Sitemap
import Green.Site.Static
import Green.Site.Templates
import Green.Template.Context

site :: SiteConfig -> Rules ()
site config = do
  brokenLinks
  images
  js config
  scss config
  templatesDependency <- templates
  rulesExtraDependencies [templatesDependency] do
    let context = customContext config
    homePage context
    pages context
    blog config context
    code
    static
    feed context
    sitemap context
    robotsTxt config context
