module Green.Lens where

import Language.Haskell.TH
import Lens.Micro
import Lens.Micro.TH

makeLensesWithL :: Name -> DecsQ
makeLensesWithL = makeLensesWith $ lensRules & lensField .~ lensFieldL
  where
    lensFieldL _ _ n = [TopName (mkName (nameBase n ++ "L"))]
