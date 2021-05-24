module Site.Rule.BlogSpec where

import Site.Rule.Blog
import Site.Spec.Common

spec :: Spec
spec = do
  around withRunRoutes do
    describe "dateRoute" do
      runRoutesTable dateRoute $
        [ ("blog/2012-11-07-this-one.md", Just "blog/2012/11/07/this-one.md"),
          ("blog/2012-11-16-that-one.md", Just "blog/2012/11/16/that-one.md"),
          ("blog/not-this-one.md", Just "blog/not-this-one.md")
        ]
    describe "publishedPostRoute" do
      runRoutesTable publishedPostRoute $
        [ ("blog/2012-11-07-this-one.md", Just "blog/2012/11/07/this-one/index.html"),
          ("blog/2012-11-16-that-one.md", Just "blog/2012/11/16/that-one/index.html"),
          ("blog/not-this-one.md", Nothing)
        ]
    describe "draftPostRoute" do
      runRoutesTable draftPostRoute $
        [ ("blog/2012-11-07-this-one.md", Just "drafts/2012/11/07/this-one/index.html"),
          ("blog/2012-11-16-that-one.md", Just "drafts/2012/11/16/that-one/index.html"),
          ("blog/not-this-one.md", Nothing)
        ]
