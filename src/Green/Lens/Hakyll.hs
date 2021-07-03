module Green.Lens.Hakyll where

import Green.Lens.TH
import Hakyll

makeLensesWithL ''Configuration
makeLensesWithL ''FeedConfiguration
