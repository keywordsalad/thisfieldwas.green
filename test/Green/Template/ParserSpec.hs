module Green.Template.ParserSpec where

import Data.List.NonEmpty as NEL
import Green.Template
import Green.Template.Structural
import Green.TestSupport
import Text.Parsec hiding (runParser, runParserT, token, tokens)

spec :: Spec
spec = do
  withParser "tokens" (tokens `readingFromString`) do
    context "expression mode" do
      "{{ a b c d"
        `produces` [ OpenBlockToken',
                     NameToken' "a",
                     NameToken' "b",
                     NameToken' "c",
                     NameToken' "d"
                   ]
      "{{ 1 2 3 4"
        `produces` [ OpenBlockToken',
                     IntToken' 1,
                     IntToken' 2,
                     IntToken' 3,
                     IntToken' 4
                   ]
      "{{ a 1 b 2"
        `produces` [ OpenBlockToken',
                     NameToken' "a",
                     IntToken' 1,
                     NameToken' "b",
                     IntToken' 2
                   ]
      "{{ 1.1 2.2 3.3 4.4"
        `produces` [ OpenBlockToken',
                     DoubleToken' 1.1,
                     DoubleToken' 2.2,
                     DoubleToken' 3.3,
                     DoubleToken' 4.4
                   ]

      context "invalid numbers" do
        "{{ 1.a 2.2"
          `rejectsWith` "\"tokens\" (line 1, column 6):\nunexpected \"a\"\nexpecting digit"
        "{{ 1a 2.2"
          `rejectsWith` "\"tokens\" (line 1, column 6):\nunexpected 'a'"

      context "keywords and word literals" do
        "{{ true else false end"
          `produces` [ OpenBlockToken',
                       BoolToken' True,
                       ElseToken',
                       BoolToken' False,
                       EndToken'
                     ]

      context "symbols" do
        "{{ { } ( ) # @ | : . , }}"
          `produces` [ OpenBlockToken',
                       OpenBraceToken',
                       CloseBraceToken',
                       OpenParenToken',
                       CloseParenToken',
                       OpenTemplateToken',
                       OpenLayoutToken',
                       PipeToken',
                       ColonToken',
                       DotToken',
                       CommaToken',
                       CloseBlockToken'
                     ]

      context "string literals" do
        "{{ \"abcd\""
          `produces` [OpenBlockToken', StringToken' "abcd"]
        "{{ \"\\\"abcd\""
          `produces` [OpenBlockToken', StringToken' "\"abcd"]
        "{{ \"a\\'b\\nc\\td\\\\\""
          `produces` [OpenBlockToken', StringToken' "a\'b\nc\td\\"]
        "{{ \"this string }} has these {{\""
          `produces` [OpenBlockToken', StringToken' "this string }} has these {{"]

      context "text" do
        "\\}}" `produces` [TextToken' "}}"]
        "\\{{" `produces` [TextToken' "{{"]
        "\\{{!" `produces` [TextToken' "{{!"]
        "{a" `produces` [TextToken' "{a"]
        "}a" `produces` [TextToken' "}a"]
        "\\a" `produces` [TextToken' "\\a"]
        "\\\\" `produces` [TextToken' "\\\\"]

        "this contains a \\}} literal"
          `produces` [TextToken' "this contains a }} literal"]

      context "comment blocks" do
        "{{! this is a comment }}"
          `produces` [ OpenBlockToken',
                       OpenCommentToken',
                       TextToken' " this is a comment ",
                       CloseBlockToken'
                     ]
        "{{! this {{ is {{! a }} nested }} comment }}"
          `produces` [ OpenBlockToken',
                       OpenCommentToken',
                       TextToken' " this {{ is {{! a }} nested }} comment ",
                       CloseBlockToken'
                     ]
        "{{! this has an \\}} in it }}"
          `produces` [ OpenBlockToken',
                       OpenCommentToken',
                       TextToken' " this has an }} in it ",
                       CloseBlockToken'
                     ]

      context "mixing blocks" do
        "{{ expression }} and some text"
          `produces` [ OpenBlockToken',
                       NameToken' "expression",
                       CloseBlockToken',
                       TextToken' " and some text"
                     ]
        "this {{ expression }} and some text"
          `produces` [ TextToken' "this ",
                       OpenBlockToken',
                       NameToken' "expression",
                       CloseBlockToken',
                       TextToken' " and some text"
                     ]
        "here: {{! a comment {{ with nesting }} }} followed by {{ an | expression }}"
          `produces` [ TextToken' "here: ",
                       OpenBlockToken',
                       OpenCommentToken',
                       TextToken' " a comment {{ with nesting }} ",
                       CloseBlockToken',
                       TextToken' " followed by ",
                       OpenBlockToken',
                       NameToken' "an",
                       PipeToken',
                       NameToken' "expression",
                       CloseBlockToken'
                     ]

  withParser "parseBlocks" (blocks `readingFromTokens`) do
    context "expression blocks" do
      "{{ a | b }}{{ c d }}"
        `produces` [ ExpressionBlock'
                       ( FilterExpression'
                           (NameExpression' "a")
                           (NameExpression' "b")
                       ),
                     ExpressionBlock'
                       ( ApplyExpression'
                           (NameExpression' "c")
                           (NameExpression' "d")
                       )
                   ]
      "{{ thing | withContext { this: a, that: b } }}"
        `produces` [ ExpressionBlock'
                       ( FilterExpression'
                           (NameExpression' "thing")
                           ( ApplyExpression'
                               (NameExpression' "withContext")
                               ( ContextExpression'
                                   [ ("this", NameExpression' "a"),
                                     ("that", NameExpression' "b")
                                   ]
                               )
                           )
                       )
                   ]
      "{{ (a (b | c)) }}"
        `produces` [ ExpressionBlock'
                       ( ApplyExpression'
                           (NameExpression' "a")
                           ( FilterExpression'
                               (NameExpression' "b")
                               (NameExpression' "c")
                           )
                       )
                   ]
      "{{ [1, 2, 3] }}"
        `produces` [ ExpressionBlock'
                       ( ListExpression'
                           [ IntExpression' 1,
                             IntExpression' 2,
                             IntExpression' 3
                           ]
                       )
                   ]
      "{{ [] }}"
        `produces` [ExpressionBlock' (ListExpression' [])]

    context "comment blocks" do
      "{{! this is a comment }}"
        `produces` [CommentBlock' " this is a comment "]

    context "template blocks" do
      unlines
        [ "{{# if (this thing) }}",
          "then show this thing",
          "{{ else some condition }}",
          "then show this other thing",
          "{{ else }}",
          "otherwise show this thing",
          "{{ end }}"
        ]
        `produces` [ TemplateStartBlock'
                       ( ApplyExpression'
                           (NameExpression' "if")
                           ( ApplyExpression'
                               (NameExpression' "this")
                               (NameExpression' "thing")
                           )
                       ),
                     TextBlock' "\nthen show this thing\n",
                     TemplateNextBlock'
                       ( ApplyExpression'
                           (NameExpression' "some")
                           (NameExpression' "condition")
                       ),
                     TextBlock' "\nthen show this other thing\n",
                     TemplateElseBlock',
                     TextBlock' "\notherwise show this thing\n",
                     TemplateEndBlock',
                     TextBlock' "\n"
                   ]

    context "layout blocks" do
      unlines
        [ "{{@ layout \"file.md\" | withContext { title: \"food\" } }}",
          "this gets wrapped with file.md"
        ]
        `produces` [ LayoutBlock'
                       ( FilterExpression'
                           ( ApplyExpression'
                               (NameExpression' "layout")
                               (StringExpression' "file.md")
                           )
                           ( ApplyExpression'
                               (NameExpression' "withContext")
                               ( ContextExpression'
                                   [ ("title", StringExpression' "food")
                                   ]
                               )
                           )
                       ),
                     TextBlock' "\nthis gets wrapped with file.md\n"
                   ]

  context "templates" do
    withParser "startTemplate" (startTemplate `readingFromBlocks`) do
      "{{# if (this thing) }} text {{ end }}"
        `produces` TemplateApplyBlock'
          ( ApplyExpression'
              (NameExpression' "if")
              ( ApplyExpression'
                  (NameExpression' "this")
                  (NameExpression' "thing")
              )
          )
          (Template' [TextBlock' " text "])

      unlines
        [ "{{# if (that thing) }}",
          "text",
          "{{ end }}"
        ]
        `produces` TemplateApplyBlock'
          ( ApplyExpression'
              (NameExpression' "if")
              ( ApplyExpression'
                  (NameExpression' "that")
                  (NameExpression' "thing")
              )
          )
          (Template' [TextBlock' "\ntext\n"])

      unlines
        [ "{{# if this }}",
          "that",
          "{{ else and }}",
          "the other",
          "{{ end }}"
        ]
        `produces` TemplateApplyBlock'
          ( ApplyExpression'
              (NameExpression' "if")
              (NameExpression' "this")
          )
          (Template' [TextBlock' "\nthat\n"])

      unlines
        [ "{{# if this }}",
          "that",
          "{{ else }}",
          "the other",
          "{{ end }}"
        ]
        `produces` TemplateApplyBlock'
          ( ApplyExpression'
              (NameExpression' "if")
              (NameExpression' "this")
          )
          (Template' [TextBlock' "\nthat\n"])

    withParser "nextTemplate" (nextTemplate `readingFromBlocks`) do
      "{{ else this }} that {{ end }}"
        `produces` TemplateApplyBlock'
          (NameExpression' "this")
          (Template' [TextBlock' " that "])

      unlines
        [ "{{ else then }}",
          "this thing",
          "{{ else otherwise }}"
        ]
        `produces` TemplateApplyBlock'
          (NameExpression' "then")
          (Template' [TextBlock' "\nthis thing\n"])

      unlines
        [ "{{ else then }}",
          "this thing",
          "{{ else }}"
        ]
        `produces` TemplateApplyBlock'
          (NameExpression' "then")
          (Template' [TextBlock' "\nthis thing\n"])

      unlines
        [ "{{ else then }}",
          "this thing",
          "{{ end }}"
        ]
        `produces` TemplateApplyBlock'
          (NameExpression' "then")
          (Template' [TextBlock' "\nthis thing\n"])

    withParser "defaultTemplate" (defaultTemplate `readingFromBlocks`) do
      "{{ else }} do this {{ end }}"
        `produces` TemplateDefaultBlock' (Template' [TextBlock' " do this "])

      unlines
        [ "{{ else }}",
          "do this",
          "{{ end }}"
        ]
        `produces` TemplateDefaultBlock' (Template' [TextBlock' "\ndo this\n"])

  withParser "structures" (structures `readingFromBlocks`) do
    context "template blocks" do
      unlines
        [ "{{# if (this thing) }}",
          "then show this thing",
          "{{ else some condition }}",
          "then show this other thing",
          "{{ else }}",
          "otherwise show this thing",
          "{{ end }}"
        ]
        `produces` [ TemplateBlock'
                       ( NEL.fromList
                           [ TemplateApplyBlock'
                               ( ApplyExpression'
                                   (NameExpression' "if")
                                   ( ApplyExpression'
                                       (NameExpression' "this")
                                       (NameExpression' "thing")
                                   )
                               )
                               (Template' [TextBlock' "\nthen show this thing\n"]),
                             TemplateApplyBlock'
                               ( ApplyExpression'
                                   (NameExpression' "some")
                                   (NameExpression' "condition")
                               )
                               (Template' [TextBlock' "\nthen show this other thing\n"])
                           ]
                       )
                       (Just $ TemplateDefaultBlock' (Template' [TextBlock' "\notherwise show this thing\n"])),
                     TextBlock' "\n"
                   ]

tokens :: Lexer [Token]
tokens = many token <* eof

blocks :: TokenParser [Block]
blocks = many block <* eof

structures :: BlockParser [Block]
structures = many structure <* eof

type TestParser s a = s -> Either ParseError a

withParser :: String -> (String -> TestParser s a) -> SpecWith (TestParser s a) -> Spec
withParser parserName parser specs =
  context parserName do
    around (bracket (return (parser parserName)) (const (return ()))) do
      specs

readingFromString :: Lexer a -> String -> TestParser String a
readingFromString = runParser

readingFromTokens :: TokenParser a -> String -> TestParser String a
readingFromTokens p source =
  runParser tokens source
    >=> runParser p source

debugReadingFromTokens :: TokenParser a -> String -> TestParser String a
debugReadingFromTokens p source =
  runParser tokens source
    >=> debugRunParser p source

readingFromBlocks :: BlockParser a -> String -> TestParser String a
readingFromBlocks p source =
  runParser tokens source
    >=> runParser blocks source
    >=> runParser p source

debugReadingFromBlocks :: BlockParser a -> String -> TestParser String a
debugReadingFromBlocks p source =
  runParser tokens source
    >=> runParser blocks source
    >=> debugRunParser p source

produces :: (Show s, Eq b, Show b, Structural a b) => s -> b -> SpecWith (TestParser s a)
produces input expected =
  describe (show input) do
    it ("produces " ++ show expected) \p -> do
      second toStructure (p input) `shouldBe` Right expected

rejectsWith :: (Show s, Eq a, Show a) => s -> String -> SpecWith (TestParser s a)
rejectsWith input expected =
  describe (show input) do
    it ("rejects tokens with " ++ show expected) \p -> do
      first show (p input) `shouldBe` Left expected

infixr 0 `produces`, `rejectsWith`
