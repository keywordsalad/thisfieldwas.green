module Site.TestSupport
  ( module Site.Common,
    module Site.TestSupport,
    module Site.TestSupport.Compiler,
    module Site.TestSupport.Config,
    module Site.TestSupport.Resource,
    module Site.TestSupport.Routes,
    module Site.TestSupport.TestEnv,
    module Test.Hspec,
  )
where

import Site.Common
import Site.TestSupport.Compiler
import Site.TestSupport.Config
import Site.TestSupport.Resource
import Site.TestSupport.Routes
import Site.TestSupport.TestEnv
import Test.Hspec

runAll :: [SpecWith a] -> SpecWith a
runAll = sequenceA_
