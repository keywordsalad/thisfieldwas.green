module Green.Site.BlogSpec where

import Green.Site.Blog
import Green.TestSupport

spec :: Spec
spec = do
  around (withDefaultTestEnv `providing` runRouteSpec) do
    describe "dateRoute" do
      runRouteExamples dateRoute $
        [ ("2012-11-07-this-one.md", Just "2012/11/07/this-one.md"),
          ("2012-11-16-that-one.md", Just "2012/11/16/that-one.md"),
          ("not-this-one.md", Just "not-this-one.md"),
          ("underwater-basketry/2012-11-07-this-one.md", Just "underwater-basketry/2012/11/07/this-one.md")
        ]
    describe "postsRoute" do
      runRouteExamples postsRoute $
        [ ("_posts/2012-11-07-this-one.md", Just "blog/2012/11/07/this-one/index.html"),
          ("_posts/2012-11-16-that-one.md", Just "blog/2012/11/16/that-one/index.html"),
          ("_posts/not-this-one.md", Just "blog/not-this-one/index.html"),
          ("_posts/underwater-basketry/2012-11-07-this-one.md", Just "blog/underwater-basketry/2012/11/07/this-one/index.html")
        ]
    describe "draftsRoute" do
      runRouteExamples draftsRoute $
        [ ("_drafts/this-one.md", Just "drafts/this-one/index.html"),
          ("_drafts/that-one.md", Just "drafts/that-one/index.html"),
          ("_drafts/underwater-basketry/this-one.md", Just "drafts/underwater-basketry/this-one/index.html")
        ]
