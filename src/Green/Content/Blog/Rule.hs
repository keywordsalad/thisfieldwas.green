module Green.Content.Blog.Rule where

import Green.Common
import Green.Content.Blog.Compiler
import Green.Content.Blog.Route
import Green.Route
import Green.Template

blogRules :: Context String -> Rules ()
blogRules context =
  let rules =
        [ blogIndexRules,
          archiveRules,
          draftArchiveRules,
          postRules,
          draftRules
        ]
   in sequenceA_ $ rules <*> pure context

blogIndexRules :: Context String -> Rules ()
blogIndexRules context =
  match "blog/index.html" do
    route idRoute
    compile $ blogCompiler context

archiveRules :: Context String -> Rules ()
archiveRules context = do
  match "blog/archives.html" do
    route indexRoute
    compile $ archivesCompiler context

draftArchiveRules :: Context String -> Rules ()
draftArchiveRules context = do
  match "blog/drafts.html" do
    route indexRoute
    compile $ draftArchivesCompiler context

postRules :: Context String -> Rules ()
postRules localConfig = do
  match "_posts/**" do
    route postRoute
    compile $ postCompiler localConfig

draftRules :: Context String -> Rules ()
draftRules localConfig = do
  match "_drafts/**" do
    route draftRoute
    compile $ draftCompiler localConfig
