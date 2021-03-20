import Hakyll
import Route

main :: IO ()
main = hakyllWith siteConfig do
  cssRoute
  sassRoute
  jsRoute
  imagesRoute
  pagesRoute
  postsRoute
  archiveRoute
  indexRoute
  templatesRoute

siteConfig :: Configuration
siteConfig =
  defaultConfiguration
    { providerDirectory = "site-src",
      destinationDirectory = "gh-pages"
    }
