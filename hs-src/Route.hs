module Route where

import Context
import Hakyll
import Hakyll.Web.Sass
import Js

imagesRoute :: Rules ()
imagesRoute = match "images/*" do
  route idRoute
  compile copyFileCompiler

cssRoute :: Rules ()
cssRoute = match "css/**.css" do
  route idRoute
  compile compressCssCompiler

jsRoute :: Rules ()
jsRoute = match "js/**.js" do
  route idRoute
  compile compressJsCompiler

sassRoute :: Rules ()
sassRoute = do
  sassDependency <- makePatternDependency "css/**.sass"
  rulesExtraDependencies [sassDependency] $ match "css/main.sass" do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

pagesRoute :: Rules ()
pagesRoute = match (fromList ["about.rst", "contact.md"]) do
  route $ setExtension "html"
  compile $
    pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

postsRoute :: Rules ()
postsRoute = match "posts/*" do
  route $ setExtension "html"
  compile $
    pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

archiveRoute :: Rules ()
archiveRoute = create ["archive.html"] do
  route idRoute
  compile do
    posts <- recentFirst =<< loadAll "posts/*"
    makeItem ""
      >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx posts)
      >>= loadAndApplyTemplate "templates/default.html" (archiveCtx posts)
      >>= relativizeUrls

indexRoute :: Rules ()
indexRoute = match "index.html" do
  route idRoute
  compile do
    posts <- recentFirst =<< loadAll "posts/*"
    let indexCtx =
          listField "posts" postCtx (return posts)
            `mappend` defaultContext

    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/default.html" indexCtx
      >>= relativizeUrls

templatesRoute :: Rules ()
templatesRoute = match "templates/*" $ compile templateBodyCompiler
