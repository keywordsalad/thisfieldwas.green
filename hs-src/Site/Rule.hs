module Site.Rule where

import Hakyll
import Site.Rule.Archive
import Site.Rule.Feed
import Site.Rule.Index
import Site.Rule.Js
import Site.Rule.Page
import Site.Rule.Post
import Site.Rule.Robot
import Site.Rule.Sass
import Site.Rule.Sitemap

rules :: [(String, String)] -> FeedConfiguration -> Context String -> Rules ()
rules env feedConfig baseCtx = do
  postRules env baseCtx
  feedRules feedConfig baseCtx
  indexRules baseCtx
  pageRules baseCtx
  robotsTxtRules baseCtx
  archiveRules baseCtx
  sitemapRules baseCtx
  configRules
  imageRules
  cssRules
  templateRules
  downloadRules
  jsRules
  sassRules

configRules :: Rules ()
configRules =
  match (fromList [".nojekyll", "CNAME"]) do
    route idRoute
    compile copyFileCompiler

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
templateRules =
  match "templates/*" $ compile templateBodyCompiler

downloadRules :: Rules ()
downloadRules =
  match "downloads/**" do
    route idRoute
    compile copyFileCompiler
