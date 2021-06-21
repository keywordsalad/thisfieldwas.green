module GreenSite.Lens.Hakyll where

import GreenSite.Lens
import Hakyll

makeLensesWithL ''Configuration
makeLensesWithL ''FeedConfiguration
