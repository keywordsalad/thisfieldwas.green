module Site.Rule.BlogSpec where

import Site.Rule.Blog
import Site.TestSupport

spec :: Spec
spec = do
  around (withDefaultTestEnv `providing` runRouteSpec) do
    describe "dateRoute" do
      runRouteExamples dateRoute $
        [ ("posts/2012-11-07-this-one.md", Just "posts/2012/11/07/this-one.md"),
          ("posts/2012-11-16-that-one.md", Just "posts/2012/11/16/that-one.md"),
          ("posts/not-this-one.md", Just "posts/not-this-one.md"),
          ("posts/underwater-basketry/2012-11-07-this-one.md", Just "posts/underwater-basketry/2012/11/07/this-one.md")
        ]
    describe "basePostRoute" do
      runRouteExamples basePostRoute $
        [ ("posts/2012-11-07-this-one.md", Just "posts/2012/11/07/this-one/index.html"),
          ("posts/2012-11-16-that-one.md", Just "posts/2012/11/16/that-one/index.html"),
          ("posts/not-this-one.md", Nothing),
          ("posts/underwater-basketry/2012-11-07-this-one.md", Just "posts/underwater-basketry/2012/11/07/this-one/index.html")
        ]
    describe "publishedPostRoute" do
      runRouteExamples publishedPostRoute $
        [ ("posts/2012-11-07-this-one.md", Just "blog/2012/11/07/this-one/index.html"),
          ("posts/2012-11-16-that-one.md", Just "blog/2012/11/16/that-one/index.html"),
          ("posts/not-this-one.md", Nothing),
          ("posts/underwater-basketry/2012-11-07-this-one.md", Just "blog/underwater-basketry/2012/11/07/this-one/index.html")
        ]
    describe "draftPostRoute" do
      runRouteExamples draftPostRoute $
        [ ("posts/2012-11-07-this-one.md", Just "blog/drafts/2012/11/07/this-one/index.html"),
          ("posts/2012-11-16-that-one.md", Just "blog/drafts/2012/11/16/that-one/index.html"),
          ("posts/not-this-one.md", Nothing),
          ("posts/underwater-basketry/2012-11-07-this-one.md", Just "blog/drafts/underwater-basketry/2012/11/07/this-one/index.html")
        ]
