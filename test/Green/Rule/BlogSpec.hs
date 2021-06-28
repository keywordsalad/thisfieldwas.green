module Green.Rule.BlogSpec where

import Green.Rule.Blog
import Green.TestSupport

spec :: Spec
spec = do
  around (withDefaultTestEnv `providing` runRouteSpec) do
    describe "dateRoute" do
      runRouteExamples dateRoute $
        [ ("_drafts/2012-11-07-this-one.md", Just "posts/2012/11/07/this-one.md"),
          ("_drafts/2012-11-16-that-one.md", Just "posts/2012/11/16/that-one.md"),
          ("_drafts/not-this-one.md", Just "posts/not-this-one.md"),
          ("_drafts/underwater-basketry/2012-11-07-this-one.md", Just "posts/underwater-basketry/2012/11/07/this-one.md")
        ]
    describe "postRoute" do
      runRouteExamples postRoute $
        [ ("_posts/2012-11-07-this-one.md", Just "blog/2012/11/07/this-one/index.html"),
          ("_posts/2012-11-16-that-one.md", Just "blog/2012/11/16/that-one/index.html"),
          ("_posts/not-this-one.md", Nothing),
          ("_posts/underwater-basketry/2012-11-07-this-one.md", Just "blog/underwater-basketry/2012/11/07/this-one/index.html")
        ]
    describe "draftRoute" do
      runRouteExamples draftRoute $
        [ ("_drafts/this-one.md", Just "blog/drafts/this-one/index.html"),
          ("_drafts/that-one.md", Just "blog/drafts/that-one/index.html"),
          ("_drafts/underwater-basketry/this-one.md", Just "blog/drafts/underwater-basketry/this-one/index.html")
        ]
