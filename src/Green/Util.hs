module Green.Util where

import Control.Applicative ((<|>))
import Data.Char
import Data.Foldable (sequenceA_)
import Data.String.Utils as S
import Hakyll
import Lens.Micro
import System.FilePath (splitFileName, takeDirectory)

dropIndex :: FilePath -> FilePath
dropIndex url = case splitFileName url of
  (p, "index.html") -> takeDirectory p
  _ -> url

timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S%Z"

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

firstMaybe :: (Foldable m) => m (Maybe a) -> Maybe a
firstMaybe = foldl (<|>) Nothing

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)
