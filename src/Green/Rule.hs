module Green.Rule where

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
import Hakyll

rules :: SiteConfig -> Rules ()
rules config = do
  brokenLinks
  configRules
  imageRules
  jsRules
  cssRules config
  downloadRules
  codeDependency <- codeRules
  rulesExtraDependencies [codeDependency] do
    blogRules env baseCtx
    feedRules feedConfig baseCtx
    indexRules env baseCtx
    pageRules baseCtx
    robotsTxtRules baseCtx
    archiveRules baseCtx
    sitemapRules baseCtx
    brokenLinks

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

templateRules :: Rules ()
templateRules = do
  match "_layouts/**" $ compile layoutCompiler
  match "_partials/**" $ compile templateCompiler
  match "_templates/**" $ compile templateCompiler
