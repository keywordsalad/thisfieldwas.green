module Green.Lens
  ( module Green.Lens,
    module Green.Lens.Hakyll,
    module Green.Lens.TH,
  )
where

import Green.Common
import Green.Lens.Hakyll
import Green.Lens.TH

(~<>) :: (Monoid a) => ASetter s t a a -> a -> s -> t
(~<>) l a = over l (`mappend` a)
{-# INLINE (~<>) #-}

infixr 4 ~<>
