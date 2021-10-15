module Green.Content.Blog.Context where

import Green.Content.Blog.Compiler
import Green.Template

blogContext :: Context String
blogContext = teaserField "teaser" publishedPostsSnapshot
