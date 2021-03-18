import Hakyll
import Hakyll.Web.Sass
import Js

main :: IO ()
main = hakyll do
  match "images/*" do
    route   idRoute
    compile copyFileCompiler

  match "css/**.css" do
    route   idRoute
    compile compressCssCompiler

  match "js/**.js" do
    route   idRoute
    compile compressJsCompiler

  sassDependency <- makePatternDependency "css/**.sass"
  rulesExtraDependencies [sassDependency]
    $ match "css/main.sass" do
      route $ setExtension "css"
      let compressCssItem = fmap compressCss
      compile (compressCssItem <$> sassCompiler)

  match (fromList ["about.rst", "contact.md"]) do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "posts/*" do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] do
    route idRoute
    compile do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts) `mappend`
            constField "title" "Archives" `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" do
    route idRoute
    compile do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts) `mappend`
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  <> defaultContext
