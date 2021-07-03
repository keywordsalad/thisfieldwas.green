module Green.Lens.TH where

import Green.Common
import Language.Haskell.TH

makeLensesWithL :: Name -> DecsQ
makeLensesWithL = makeLensesWith $ lensRules & lensField .~ lensFieldL
  where
    lensFieldL _ _ n = [TopName (mkName (nameBase n ++ "L"))]
