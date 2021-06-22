module Green.Lens.Hakyll where

import Green.Lens
import Hakyll

makeLensesWithL ''Configuration
makeLensesWithL ''FeedConfiguration
