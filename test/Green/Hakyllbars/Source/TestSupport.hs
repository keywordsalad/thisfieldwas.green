module Green.Hakyllbars.Source.TestSupport where

import Control.Exception
import Data.Bifunctor
import Green.Hakyllbars.AstStructure
import Test.Hspec
import Text.Parsec hiding (runParser, runParserT, token, tokens)

type Parse s a = s -> Either ParseError a

withParser :: (String -> Parse s a) -> SpecWith (Parse s a) -> Spec
withParser parser =
  around (bracket (return (parser "test")) (const (return ())))

produces :: (Show s, Eq b, Show b, AstStructure a b) => s -> b -> SpecWith (Parse s a)
produces input expected =
  describe (show input) do
    it ("produces " ++ show expected) \p -> do
      second intoAstStructure (p input) `shouldBe` Right expected

rejectsWith :: (Show s, Eq a, Show a) => s -> String -> SpecWith (Parse s a)
rejectsWith input expected =
  describe (show input) do
    it ("rejects tokens with " ++ show expected) \p -> do
      first show (p input) `shouldBe` Left expected

infixr 0 `produces`, `rejectsWith`
