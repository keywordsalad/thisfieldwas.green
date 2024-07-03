module Green.Hakyllbars.Source.ParserSpec where

import Green.Hakyllbars.AstStructure
import Green.Hakyllbars.Source.TestSupport
import Green.Hakyllbars.Source
import Green.Hakyllbars.TestSupport

spec :: Spec
spec = do
  withParser parse do
    context "expression blocks" do
      "{{a | b}}{{c d}}"
        `produces` Template'
          [ ExpressionBlock'
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
      "{{thing | with { this: a, that: b } }}"
        `produces` Template'
          [ ExpressionBlock'
              ( FilterExpression'
                  (NameExpression' "thing")
                  ( ApplyExpression'
                      (NameExpression' "with")
                      ( ContextExpression'
                          [ ("this", NameExpression' "a"),
                            ("that", NameExpression' "b")
                          ]
                      )
                  )
              )
          ]
      "{{thing | with this: a, that: b}}"
        `produces` Template'
          [ ExpressionBlock'
              ( FilterExpression'
                  (NameExpression' "thing")
                  ( ApplyExpression'
                      (NameExpression' "with")
                      ( ContextExpression'
                          [ ("this", NameExpression' "a"),
                            ("that", NameExpression' "b")
                          ]
                      )
                  )
              )
          ]
      "{{(a (b | c))}}"
        `produces` Template'
          [ ExpressionBlock'
              ( ApplyExpression'
                  (NameExpression' "a")
                  ( FilterExpression'
                      (NameExpression' "b")
                      (NameExpression' "c")
                  )
              )
          ]
      "{{[1, 2, 3]}}"
        `produces` Template'
          [ ExpressionBlock'
              ( ListExpression'
                  [ IntExpression' 1,
                    IntExpression' 2,
                    IntExpression' 3
                  ]
              )
          ]
      "{{[]}}"
        `produces` Template' [ExpressionBlock' (ListExpression' [])]

    context "comment blocks" do
      "{{! this is a comment }}"
        `produces` Template' [CommentBlock' " this is a comment "]

    context "alt blocks" do
      "{{#if (this thing)}} text {{#end}}"
        `produces` Template'
          [ AltBlock'
              [ ApplyBlock'
                  ( ApplyExpression'
                      (NameExpression' "if")
                      ( ApplyExpression'
                          (NameExpression' "this")
                          (NameExpression' "thing")
                      )
                  )
                  [TextBlock' " text "]
              ]
              Nothing
          ]

      unlines
        [ "{{#if (that thing)}}",
          "text",
          "{{#end}}"
        ]
        `produces` Template'
          [ AltBlock'
              [ ApplyBlock'
                  ( ApplyExpression'
                      (NameExpression' "if")
                      ( ApplyExpression'
                          (NameExpression' "that")
                          (NameExpression' "thing")
                      )
                  )
                  [TextBlock' "\ntext\n"]
              ]
              Nothing,
            TextBlock' "\n"
          ]

      unlines
        [ "{{#if this}}",
          "that",
          "{{#else then}}",
          "the other",
          "{{#end}}"
        ]
        `produces` Template'
          [ AltBlock'
              [ ApplyBlock'
                  ( ApplyExpression'
                      (NameExpression' "if")
                      (NameExpression' "this")
                  )
                  [TextBlock' "\nthat\n"],
                ApplyBlock'
                  (NameExpression' "then")
                  [TextBlock' "\nthe other\n"]
              ]
              Nothing,
            TextBlock' "\n"
          ]

      unlines
        [ "{{#if this}}",
          "that",
          "{{#else}}",
          "the other",
          "{{#end}}"
        ]
        `produces` Template'
          [ AltBlock'
              [ ApplyBlock'
                  ( ApplyExpression'
                      (NameExpression' "if")
                      (NameExpression' "this")
                  )
                  [TextBlock' "\nthat\n"]
              ]
              (Just $ DefaultBlock' [TextBlock' "\nthe other\n"]),
            TextBlock' "\n"
          ]

      unlines
        [ "{{#if (this thing)}}",
          "then show this thing",
          "{{#else some condition}}",
          "then show this other thing",
          "{{#else}}",
          "otherwise show this thing",
          "{{#end}}"
        ]
        `produces` Template'
          [ AltBlock'
              [ ApplyBlock'
                  ( ApplyExpression'
                      (NameExpression' "if")
                      ( ApplyExpression'
                          (NameExpression' "this")
                          (NameExpression' "thing")
                      )
                  )
                  [TextBlock' "\nthen show this thing\n"],
                ApplyBlock'
                  ( ApplyExpression'
                      (NameExpression' "some")
                      (NameExpression' "condition")
                  )
                  [TextBlock' "\nthen show this other thing\n"]
              ]
              ( Just $
                  DefaultBlock'
                    [TextBlock' "\notherwise show this thing\n"]
              ),
            TextBlock' "\n"
          ]

    context "off blocks" do
      unlines
        [ "{{*",
          "{{this}} is",
          "{{!verbatim}}",
          "and {{@bananana}}",
          "-}}"
        ]
        `produces` Template'
          [TextBlock' "\n{{this}} is\n{{!verbatim}}\nand {{@bananana}}\n"]
