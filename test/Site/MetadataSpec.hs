module Site.MetadataSpec (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as B
import Site.Metadata
import Test.Hspec

spec :: Spec
spec = do
  describe "PageMetadata" do
    it "deserializes from YAML" do
      let yaml =
            "content-templates: post \n\
            \templates: default, skeleton \n\
            \title: \"App-Config-App in Action\" \n\
            \author: \"Logan McGrath\" \n\
            \date: 2012-11-20T07:00:00 CST \n\
            \comments: false \n\
            \published: true \n\
            \tags: AngularJS, Perforce, SCM, Sinatra, Configuration Management"
      return () -- TODO
