module Green.Util where

import Control.Applicative (Alternative, empty)
import Data.Char
import Data.String.Utils as S
import Green.Common

dropIndex :: FilePath -> FilePath
dropIndex url = case splitFileName url of
  (p, "index.html") -> takeDirectory p
  _ -> url

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

splitWordsBy :: (Char -> Bool) -> String -> [String]
splitWordsBy _ [] = []
splitWordsBy f xs = takeWhile (not . f) xs : splitWordsBy f (dropWhile f xs)

splitWordsByHumps :: String -> [String]
splitWordsByHumps "" = []
splitWordsByHumps xss@(x : _)
  | isUpper x =
    let prefix = takeWhile isUpper xss
        rest = dropWhile isUpper xss
     in if null rest
          then [prefix]
          else
            let currentLength = length prefix - 1
                currentWord = take currentLength prefix
                restWords = splitWordsByHumps rest
                nextWord = drop currentLength prefix ++ concat (take 1 restWords)
             in [y | y <- [currentWord, nextWord], not (null y)] ++ drop 1 restWords
  | otherwise =
    let currentWord = takeWhile (not . isUpper) xss
        rest = dropWhile (not . isUpper) xss
     in currentWord : splitWordsByHumps rest

kebabToWords :: String -> [String]
kebabToWords = splitWordsBy (== '-')

wordsToKebab :: [String] -> String
wordsToKebab = intercalate "-"

camelToWords :: String -> [String]
camelToWords = splitWordsByHumps

wordsToCamel :: [String] -> String
wordsToCamel [] = []
wordsToCamel wss = go $ fmap toLower <$> wss
  where
    go [] = []
    go (w : ws) = w ++ firstToUpper ws
    firstToUpper = concatMap \case
      (w : ws) -> toUpper w : ws
      [] -> []

snakeToWords :: String -> [String]
snakeToWords = splitWordsBy (== '_')

wordsToSnake :: [String] -> String
wordsToSnake = intercalate "_"

camelToKebab :: String -> String
camelToKebab = wordsToKebab . camelToWords

kebabToCamel :: String -> String
kebabToCamel = wordsToCamel . kebabToWords

snakeToCamel :: String -> String
snakeToCamel = wordsToCamel . snakeToWords

kebabToSnake :: String -> String
kebabToSnake = wordsToSnake . kebabToWords

snakeToKebab :: String -> String
snakeToKebab = wordsToKebab . snakeToWords

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
