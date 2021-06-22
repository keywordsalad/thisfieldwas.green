module Green.Rule where

import Green.Compiler.Layout
import Green.Config
import Green.Rule.Blog
import Green.Rule.Feed
import Green.Rule.Js
import Green.Rule.Page
import Green.Rule.Robot
import Green.Rule.Sass
import Green.Rule.Sitemap
import Hakyll

rules :: SiteConfig -> Rules ()
rules config = do
  configRules
  imageRules
  cssRules
  templateRules
  downloadRules
  jsRules
  sassRules config
  pageRules config
  robotsTxtRules config
  codeDependency <- codeRules
  rulesExtraDependencies [codeDependency] do
    blogRules config
    feedRules config
    archiveRules config
    sitemapRules config

configRules :: Rules ()
configRules =
  match (fromList [".nojekyll"]) do
    route idRoute
    compile copyFileCompiler

downloadRules :: Rules ()
downloadRules = do
  let path = "downloads/**"
  match path do
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

cssRules :: Rules ()
cssRules =
  match "css/**.css" do
    route idRoute
    compile compressCssCompiler

templateRules :: Rules ()
templateRules = do
  match "layouts/**" $ compile layoutCompiler
  match "partials/**" $ compile templateCompiler
  match "templates/**" $ compile templateCompiler
