module Green.Util where

import Control.Applicative (Alternative, empty)
import Data.Char
import Data.String.Utils as S
import Green.Common

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
kebabCase (x : xs)
  | notAllowed x = go xs
  | otherwise = toLower x : go xs
  where
    go [] = []
    go (y : ys)
      | isUpper y = '-' : toLower y : kebabCase ys
      | notAllowed y = '-' : kebabCase ys
      | otherwise = y : kebabCase ys
    notAllowed c = not (isAlphaNum c || c `elem` ("_." :: [Char]))

firstAlt :: (Foldable m, Alternative n) => m (n a) -> n a
firstAlt = foldl (<|>) empty

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
maybeHead _ = Nothing

commas :: [String] -> String
commas xs = "[" ++ intercalate ", " xs ++ "]"
