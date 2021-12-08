module Green.Template.Custom.Compiler where

import Control.Monad.State.Strict
import Data.List (nub)
import Green.Common
import Green.Template

contentCompiler :: Context String -> Item String -> Compiler (Item String)
contentCompiler context = pandocCompiler <=< applyAsTemplate context

layoutCompiler :: Context String -> Item String -> Compiler (Item String)
layoutCompiler = loadAndApplyTemplate $ fromFilePath "_layouts/from-context.html"

snapshotCompiler :: [String] -> Item String -> Compiler (Item String)
snapshotCompiler snapshots item = foldM (flip saveSnapshot) item snapshots'
  where
    snapshots' = nub ("_content" : snapshots)
