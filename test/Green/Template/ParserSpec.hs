module Green.Template.ParserSpec where

import Data.Attoparsec.Text
import Data.Bifunctor
import Data.Either
import qualified Data.Text as T
import Green.Template
import Green.TestSupport

spec :: Spec
spec = do
  describe "keyword" do
    let what = keyword "what"
    what
      `accepts` (`matchedWith` ())
      <$> ["what", "   what", "what     "]
    what
      `rejects` ["the", "   actual", "firetruck     "]

  describe "nameExpression" do
    nameExpression
      `accepts` matchedAs NameExpression
      <$> ["abc", "aBC", "_abc", "a-bc", "a01"]
    nameExpression
      `rejects` ["Abc", "09ab", "-abc"]

  describe "stringExpression" do
    stringExpression
      `accepts` as StringExpression
      <$> [ ("\"abc\"", "abc"),
            ("\"\\tthis and that\"", "\tthis and that"),
            ("\"backslashes\\\\\"", "backslashes\\"),
            ("\"\\\"\"", "\""),
            ("\"\'\"", "'")
          ]
    stringExpression
      `rejects` ["\"first\nsecond\"", "\"unclosed", "unopened\""]

  describe "scientificExperssion" do
    scientificExpression
      `accepts` [ ("123", IntExpression 123),
                  ("1", IntExpression 1),
                  ("-1", IntExpression (-1)),
                  ("+23", IntExpression 23),
                  ("1.23", DoubleExpression 1.23),
                  ("+23.55", DoubleExpression 23.55)
                ]

  describe "boolExpression" do
    boolExpression
      `accepts` as BoolExpression
      <$> [("true", True), ("false", False)]

  describe "subexpression" do
    subexpression
      `accepts` [ ("(123)", IntExpression 123),
                  ("(\"123\")", StringExpression "123"),
                  ("(12.3)", DoubleExpression 12.3),
                  ("(banana)", NameExpression "banana"),
                  ("((true))", BoolExpression True),
                  ("(false)", BoolExpression False)
                ]

  describe "contextExpression" do
    contextExpression
      `accepts` [ ( "{ this: \"that\" }",
                    ContextExpression
                      [ ("this", StringExpression "that")
                      ]
                  ),
                  ( "{partyCups: \"15.5 grapples\", bananas: 1, }",
                    ContextExpression
                      [ ("partyCups", StringExpression "15.5 grapples"),
                        ("bananas", IntExpression 1)
                      ]
                  )
                ]

  describe "simpleExpression" do
    simpleExpression
      `accepts` [ ("123", IntExpression 123),
                  ("\"123\"", StringExpression "123"),
                  ("12.3", DoubleExpression 12.3),
                  ("banana", NameExpression "banana"),
                  ("true", BoolExpression True),
                  ("(false)", BoolExpression False)
                ]

  describe "accessExpression" do
    accessExpression
      `accepts` [ ("abc", NameExpression "abc"),
                  ( "abc.def",
                    AccessExpression
                      (NameExpression "abc")
                      (NameExpression "def")
                  ),
                  ( "\"bananas\".length",
                    AccessExpression
                      (StringExpression "bananas")
                      (NameExpression "length")
                  )
                ]

  describe "applyExpression" do
    applyExpression
      `accepts` [ ("abc", NameExpression "abc"),
                  ( "abc.def",
                    AccessExpression
                      (NameExpression "abc")
                      (NameExpression "def")
                  ),
                  ( "abc def",
                    ApplyExpression
                      (NameExpression "abc")
                      (NameExpression "def")
                  ),
                  ( "abc.def \"bananas\"",
                    ApplyExpression
                      ( AccessExpression
                          (NameExpression "abc")
                          (NameExpression "def")
                      )
                      (StringExpression "bananas")
                  )
                ]

  describe "filterExpression" do
    filterExpression
      `accepts` [ ("abc", NameExpression "abc"),
                  ( "abc.def",
                    AccessExpression
                      (NameExpression "abc")
                      (NameExpression "def")
                  ),
                  ( "abc.def ghi",
                    ApplyExpression
                      ( AccessExpression
                          (NameExpression "abc")
                          (NameExpression "def")
                      )
                      (NameExpression "ghi")
                  )
                ]

parserFor :: Parser a -> String -> Either String a
parserFor p = parseOnly (p <* endOfInput) . T.pack

accepts :: (Eq a, Show a) => Parser a -> [(String, a)] -> Spec
accepts p rights' =
  sequenceA_ $
    rights' <&> \(input, output) -> it ("should match " ++ show input ++ " as " ++ show output) do
      parserFor p input `shouldBe` Right output

rejects :: (Show a) => Parser a -> [String] -> Spec
rejects p lefts' =
  sequenceA_ $
    lefts' <&> \input -> it ("should not match " ++ show input) do
      parserFor p input `shouldSatisfy` isLeft

infixr 0 `accepts`, `rejects`

matchSelf :: a -> (a, a)
matchSelf x = (x, x)

matchedWith :: a -> b -> (a, b)
matchedWith x y = (x, y)

matchedAs :: (a -> b) -> a -> (a, b)
matchedAs f x = (x, f x)

as :: (b -> c) -> (a, b) -> (a, c)
as = second
