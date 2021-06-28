module Green.Util where

import Data.Char
import Data.Foldable (sequenceA_)
import Data.Maybe
import Data.String.Utils as S
import Hakyll
import Lens.Micro

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

kebabCase :: String -> String
kebabCase [] = []
kebabCase (first : rest)
  | notAllowed first = go rest
  | otherwise = toLower first : go rest
  where
    go [] = []
    go (x : xs)
      | isUpper x = '-' : toLower x : kebabCase xs
      | notAllowed x = '-' : kebabCase xs
      | otherwise = x : kebabCase xs
    notAllowed x = not (isAlphaNum x || x `elem` ("_." :: [Char]))
