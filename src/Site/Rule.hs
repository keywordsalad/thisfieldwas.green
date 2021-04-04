module Site.Rule where

import Hakyll
import Site.Rule.Archive
import Site.Rule.Blog
import Site.Rule.Feed
import Site.Rule.Index
import Site.Rule.Js
import Site.Rule.Page
import Site.Rule.Robot
import Site.Rule.Sass
import Site.Rule.Sitemap

rules :: [(String, String)] -> FeedConfiguration -> Context String -> Rules ()
rules env feedConfig baseCtx = do
  configRules
  imageRules
  cssRules
  templateRules
  jsRules
  sassRules
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

configRules :: Rules ()
configRules =
  match (fromList [".nojekyll", "CNAME"]) do
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
  match "templates/**" $ compile templateBodyCompiler
  match "partials/**" $ compile templateBodyCompiler
