module Green.Template.Custom.Compiler where

import Data.List (nub)
import Green.Common
import Green.Template
import System.FilePath

pageCompiler :: Context String -> Item String -> Compiler (Item String)
pageCompiler = pageCompilerWithSnapshots []

pageCompilerWithSnapshots :: [String] -> Context String -> Item String -> Compiler (Item String)
pageCompilerWithSnapshots snapshots context =
  applyAsTemplate context
    >=> compilePandoc
    >=> (\x -> foldM (flip saveSnapshot) x snapshots')
    >=> applyLayout context
    >=> relativizeUrls
  where
    snapshots' =
      nub
        if "_content" `elem` snapshots
          then snapshots
          else "_content" : snapshots

applyLayout :: Context String -> Item String -> Compiler (Item String)
applyLayout context item = do
  metadataContext <- getContext $ itemIdentifier item
  unContext metadataContext "layout" item >>= \case
    StringValue layoutName -> do
      let layoutPath = "_layouts" </> layoutName <.> "html"
      layoutTemplate <- loadTemplateBody $ fromFilePath layoutPath
      applyTemplate layoutTemplate context item
    _ -> do
      debugCompiler $ "Did not receive String layout key for " ++ show (itemIdentifier item)
      return item
