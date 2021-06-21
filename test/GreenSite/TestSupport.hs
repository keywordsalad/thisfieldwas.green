module GreenSite.TestSupport
  ( module GreenSite.Common,
    module GreenSite.TestSupport,
    module GreenSite.TestSupport.Compiler,
    module GreenSite.TestSupport.Config,
    module GreenSite.TestSupport.Resource,
    module GreenSite.TestSupport.Routes,
    module GreenSite.TestSupport.TestEnv,
    module Test.Hspec,
  )
where

import GreenSite.Common
import GreenSite.TestSupport.Compiler
import GreenSite.TestSupport.Config
import GreenSite.TestSupport.Resource
import GreenSite.TestSupport.Routes
import GreenSite.TestSupport.TestEnv
import Test.Hspec

runAll :: [SpecWith a] -> SpecWith a
runAll = sequenceA_
