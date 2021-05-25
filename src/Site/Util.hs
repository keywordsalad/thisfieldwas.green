module Site.Util where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Foldable (sequenceA_)
import Data.Maybe
import Data.String.Utils as S
import Hakyll
import Lens.Micro
import qualified Text.HTML.TagSoup as TS

timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S%Z"

-- | Feed renderer signature
type RenderFeed =
  Context String -> -- Item context
  [Item String] -> -- Feed items
  Compiler (Item String) -- Resulting feed

itemSiteRoot :: Item a -> Compiler String
itemSiteRoot = fmap (toSiteRoot . fromJust) . getRoute . itemIdentifier

stripSuffix :: String -> String -> String
stripSuffix suffix text =
  if drop prefixLength text == suffix
    then prefix
    else text
  where
    prefixLength = length text - length suffix
    prefix = take prefixLength text

splitAndStrip ::
  -- | The delimiter to split the input string on.
  String ->
  -- | The string to split.
  String ->
  -- | The list of strings split from the input.
  [String]
splitAndStrip d = fmap S.strip . S.split d

sequenceRules :: [Rules ()] -> Rules ()
sequenceRules = sequenceA_

-- | Prepend a value monoidally to the lense and set the result
(~<>) :: (Monoid a) => ASetter s t a a -> a -> s -> t
(~<>) l a = over l (`mappend` a)
{-# INLINE (~<>) #-}

infixr 4 ~<>

demoteHeadersBy :: Int -> String -> String
demoteHeadersBy amount = withTags $ \case
  TS.TagOpen t a -> TS.TagOpen (demote t) a
  TS.TagClose t -> TS.TagClose (demote t)
  t -> t
  where
    demote t@['h', n]
      | isDigit n = ['h', intToDigit (min 6 $ digitToInt n + amount)]
      | otherwise = t
    demote t = t
