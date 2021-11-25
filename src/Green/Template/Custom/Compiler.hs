module Green.Template.Custom.Compiler where

import Control.Monad.State.Strict
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
  where
    snapshots' = nub ("_content" : snapshots)

applyLayout :: Context String -> Item String -> Compiler (Item String)
applyLayout context item = do
  getMetadataField "layout" item >>= \case
    StringValue layoutName -> do
      let layoutId = fromFilePath $ "_layouts" </> layoutName <.> "html"
      loadAndApplyTemplate layoutId context item
    _ -> do
      debugCompiler $ "Did not receive String layout key for " ++ show (itemIdentifier item)
      return item
