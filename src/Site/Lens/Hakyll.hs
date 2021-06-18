module Site.Lens.Hakyll where

import Hakyll
import Site.Lens

makeLensesWithL ''Configuration
makeLensesWithL ''FeedConfiguration
