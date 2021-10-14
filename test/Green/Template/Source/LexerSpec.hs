module Green.Template.Source.LexerSpec where

import Green.Template.AstStructure
import Green.Template.Source.Lexer
import Green.Template.Source.TestSupport
import Green.TestSupport
import Prelude hiding (lex)

spec :: Spec
spec = do
  withParser lex do
    context "expression mode" do
      "{{a b c d"
        `produces` [ ExpressionBlockToken',
                     NameToken' "a",
                     NameToken' "b",
                     NameToken' "c",
                     NameToken' "d"
                   ]
      "{{1 2 3 4"
        `produces` [ ExpressionBlockToken',
                     IntToken' 1,
                     IntToken' 2,
                     IntToken' 3,
                     IntToken' 4
                   ]
      "{{a 1 b 2"
        `produces` [ ExpressionBlockToken',
                     NameToken' "a",
                     IntToken' 1,
                     NameToken' "b",
                     IntToken' 2
                   ]
      "{{1.1 2.2 3.3 4.4"
        `produces` [ ExpressionBlockToken',
                     DoubleToken' 1.1,
                     DoubleToken' 2.2,
                     DoubleToken' 3.3,
                     DoubleToken' 4.4
                   ]

      context "invalid numbers" do
        "{{1.a 2.2" `rejectsWith` "\"test\" (line 1, column 5):\nunexpected \"a\"\nexpecting digit"
        "{{1a 2.2" `rejectsWith` "\"test\" (line 1, column 5):\nunexpected 'a'"

      context "keywords and word literals" do
        "{{true else false end"
          `produces` [ ExpressionBlockToken',
                       BoolToken' True,
                       ElseToken',
                       BoolToken' False,
                       EndToken'
                     ]

      context "string literals" do
        "{{\"abcd\""
          `produces` [ExpressionBlockToken', StringToken' "abcd"]
        "{{\"\\\"abcd\""
          `produces` [ExpressionBlockToken', StringToken' "\"abcd"]
        "{{\"a\\'b\\nc\\td\\\\\""
          `produces` [ExpressionBlockToken', StringToken' "a\'b\nc\td\\"]
        "{{\"this string}} has these {{\""
          `produces` [ExpressionBlockToken', StringToken' "this string}} has these {{"]
        "{{'abcd'"
          `produces` [ExpressionBlockToken', StringToken' "abcd"]
        "{{'\\\"abcd'"
          `produces` [ExpressionBlockToken', StringToken' "\"abcd"]
        "{{'a\\\"b\\nc\\td\\\\\\\''"
          `produces` [ExpressionBlockToken', StringToken' "a\"b\nc\td\\'"]
        "{{'this string}} has these {{'"
          `produces` [ExpressionBlockToken', StringToken' "this string}} has these {{"]

      context "text" do
        "\\{{*" `produces` [TextToken' "{{*"]
        "\\{{!" `produces` [TextToken' "{{!"]
        "\\}}" `produces` [TextToken' "}}"]
        "\\{{" `produces` [TextToken' "{{"]
        "{a" `produces` [TextToken' "{a"]
        "}a" `produces` [TextToken' "}a"]
        "\\a" `produces` [TextToken' "\\a"]
        "\\\\" `produces` [TextToken' "\\\\"]

        "this contains a \\}} literal"
          `produces` [TextToken' "this contains a }} literal"]

      context "comment blocks" do
        "{{! this is a comment }}"
          `produces` [ CommentBlockToken',
                       TextToken' " this is a comment ",
                       CloseBlockToken'
                     ]

        "{{! this {{is {{! a}} nested}} comment }}"
          `produces` [ CommentBlockToken',
                       TextToken' " this {{is {{! a}} nested}} comment ",
                       CloseBlockToken'
                     ]
        "{{! this has an \\}} in it }}"
          `produces` [ CommentBlockToken',
                       TextToken' " this has an }} in it ",
                       CloseBlockToken'
                     ]

      context "mixing blocks" do
        "{{expression}} and some text"
          `produces` [ ExpressionBlockToken',
                       NameToken' "expression",
                       CloseBlockToken',
                       TextToken' " and some text"
                     ]
        "this {{expression}} and some text"
          `produces` [ TextToken' "this ",
                       ExpressionBlockToken',
                       NameToken' "expression",
                       CloseBlockToken',
                       TextToken' " and some text"
                     ]
        "here: {{! a comment {{with nesting}} }} followed by {{an | expression}}"
          `produces` [ TextToken' "here: ",
                       CommentBlockToken',
                       TextToken' " a comment {{with nesting}} ",
                       CloseBlockToken',
                       TextToken' " followed by ",
                       ExpressionBlockToken',
                       NameToken' "an",
                       PipeToken',
                       NameToken' "expression",
                       CloseBlockToken'
                     ]

      context "whitespace-trimmed blocks" do
        context "end-trimming block" do
          "{{expression-}}     some spaces"
            `produces` [ ExpressionBlockToken',
                         NameToken' "expression",
                         CloseBlockToken',
                         TextToken' "some spaces"
                       ]
        context "start-trimming block" do
          "some spaces\n   {{-expression}}"
            `produces` [ TextToken' "some spaces",
                         ExpressionBlockToken',
                         NameToken' "expression",
                         CloseBlockToken'
                       ]
