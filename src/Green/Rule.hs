module Green.Rule where

import Green.Common
import Green.Compiler.Layout
import Green.Config
import Green.Rule.Blog
import Green.Rule.BrokenLinks
import Green.Rule.Css
import Green.Rule.Feed
import Green.Rule.Index
import Green.Rule.Js
import Green.Rule.Page
import Green.Rule.Robot
import Green.Rule.Sitemap

rules :: SiteConfig -> Rules ()
rules config = do
  configDep <- configRules
  rulesExtraDependencies [configDep] do
    brokenLinks
    imageRules
    jsRules
    cssRules config
    downloadRules
    codeDep <- codeRules
    rulesExtraDependencies [codeDep] do
      templateRules
      blogRules config
      feedRules config
      indexRules config
      pageRules config
      robotsTxtRules config
      archiveRules config
      sitemapRules config
      brokenLinks

configRules :: Rules Dependency
configRules = do
  let configFile = "../config.ini"
  makePatternDependency configFile

downloadRules :: Rules ()
downloadRules = do
  match "downloads/**" do
    route idRoute
    compile copyFileCompiler

codeRules :: Rules Dependency
codeRules = do
  let path = "code/**"
  match path do
    route idRoute
    compile do
      _ <- getResourceBody >>= saveSnapshot "code"
      copyFileCompiler >>= saveSnapshot "_final"
  makePatternDependency path

imageRules :: Rules ()
imageRules =
  match "images/**" do
    route idRoute
    compile copyFileCompiler

templateRules :: Rules ()
templateRules = do
  match "_layouts/**" $ compile layoutCompiler
  match "_partials/**" $ compile templateCompiler
  match "_templates/**" $ compile templateCompiler
