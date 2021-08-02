module Green.Template.ParserSpec where

import Green.Template
import Green.TestSupport

spec :: Spec
spec = do
  describe "parseTokens" do
    "a b c d" `producesTokens` [NameToken "a", NameToken "b", NameToken "c", NameToken "d"]
    "1 2 3 4" `producesTokens` [IntToken 1, IntToken 2, IntToken 3, IntToken 4]
    "a 1 b 2" `producesTokens` [NameToken "a", IntToken 1, NameToken "b", IntToken 2]
    "1.1 2.2 3.3 4.4" `producesTokens` [DoubleToken 1.1, DoubleToken 2.2, DoubleToken 3.3, DoubleToken 4.4]
    "1.a 2.2" `rejectsTokensWith` "\"test\" (line 1, column 3):\nunexpected \"a\"\nexpecting digit"
    "1a 2.2" `rejectsTokensWith` "\"test\" (line 1, column 3):\nunexpected 'a'"
    "true else false end" `producesTokens` [BoolToken True, ElseToken, BoolToken False, EndToken]
    "{ } ( ) {{ }}, # @ !, | : . ,"
      `producesTokens` [ OpenBraceToken,
                         CloseBraceToken,
                         OpenParenToken,
                         CloseParenToken,
                         OpenBlockToken,
                         CloseBlockToken, -- }} does not trim trailing whitespace
                         CommaToken,
                         OpenTemplateToken,
                         OpenLayoutToken,
                         OpenCommentToken, -- ! does not trim trailing whitespace
                         CommaToken,
                         PipeToken,
                         ColonToken,
                         DotToken,
                         CommaToken
                       ]
    "\"abcd\"" `producesToken` StringToken "abcd"
    "\"\\\"abcd\"" `producesToken` StringToken "\"abcd"
    "\"a\\'b\\nc\\td\\\\\"" `producesToken` StringToken "a\'b\nc\td\\"

producesToken :: String -> Token -> SpecWith ()
producesToken input expected =
  describe (show input) do
    it ("produces token " ++ show expected) do
      fmap fst <$> parseTokens "test" input `shouldBe` Right [expected]

producesTokens :: String -> [Token] -> SpecWith ()
producesTokens input expected =
  describe (show input) do
    it ("produces tokens " ++ show expected) do
      fmap fst <$> parseTokens "test" input `shouldBe` Right expected

rejectsTokensWith :: String -> String -> SpecWith ()
rejectsTokensWith input expected =
  describe (show input) do
    it ("rejects tokens with " ++ show expected) do
      let actual = parseTokens "test" input
      let messages = case actual of
            Left e -> show e
            Right _ -> error "OOPS"
      messages `shouldBe` expected

infixr 0 `producesTokens`, `rejectsTokensWith`
