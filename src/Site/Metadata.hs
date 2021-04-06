module Site.Metadata where

import Data.Text
import Data.Yaml
import GHC.Generics
import Hakyll

data PostMetadata = PostMetadata
  { contentTemplates :: [Text],
    templates :: [Text],
    title :: Text,
    author :: Maybe Text,
    date :: Text,
    comments :: Bool,
    published :: Bool,
    tags :: [Text]
  } deriving stock (Generic, Show, Eq)

instance ToYaml PostMetadata
instance FromYaml PostMetadata
