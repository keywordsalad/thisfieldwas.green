module Green.Template.Custom.Compiler where

import Control.Monad.State.Strict
import Data.List (nub)
import Green.Common
import Green.Template

pageCompiler :: Context String -> Item String -> Compiler (Item String)
pageCompiler = pageCompilerWithSnapshots []

pageCompilerWithSnapshots :: [String] -> Context String -> Item String -> Compiler (Item String)
pageCompilerWithSnapshots snapshots context =
  applyAsTemplate context
    >=> compilePandoc
    >=> (\x -> foldM (flip saveSnapshot) x snapshots')
    >=> loadAndApplyTemplate (fromFilePath "_layouts/from-metadata.html") context
  where
    snapshots' = nub ("_content" : snapshots)
