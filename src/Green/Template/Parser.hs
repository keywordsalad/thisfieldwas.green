module Green.Template.Parser where

import Control.Comonad
import Control.Monad.State
import Data.List (tails)
import qualified Data.Text as T
import Lens.Micro

data TrackedCursor a = TrackedCursor
  { _cursorData :: [a],
    _cursorLine :: Int,
    _cursorColumn :: Int,
    _cursorOffset :: Int
  }

defineLenses ''TrackedCursor

instance Semigroup (TrackedCursor a) where
  left <> right = left & cursorData <>~ right ^. cursorData

instance Monoid (TrackedCursor a) where
  mempty =
    TrackedCursor
      <*> mempty
      <*> 1
      <*> 1
      <*> 0

instance Functor TrackedCursor where
  fmap f c = c & cursorData %~ fmap f

instance Applicative TrackedCursor where
  pure x = mempty & cursorData .~ pure x
  cf <*> cx = cx & cursorData %~ (cf ^. cursorData <*>)

instance Monad TrackedCursor where
  mx >>= f = mconcat $ fmap f (mx ^. cursorData)

class Cursor c where
  peek :: c a -> Maybe a
  next :: c a -> (a, c a)
  null :: c a -> Bool

instance Cursor TrackingCursor where
  peek cx =
    let xs = cx ^. cursorData
     in if null xs then Nothing else Just (head xs)
  next cx =
    let x = head (cx ^. cursorData)
        xs = tail (cx ^. cursorData)
     in case x of
       | T.pack "n"
