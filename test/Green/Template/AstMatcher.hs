module Green.Template.AstMatcher where

import Data.List.NonEmpty
import Green.Template.Ast

data Template' = Template' [Block']
  deriving stock (Show)

data Block'
  = TextBlock' String
  | ExpressionBlock' Expression'
  | CommentBlock' String
  | LayoutBlock' Expression'
  | TemplateStartBlock' Expression'
  | TemplateNextBlock' Expression'
  | TemplateElseBlock'
  | TemplateEndBlock'
  | LayoutApplyBlock' Expression' Template'
  | TemplateBlock' (NonEmpty TemplateApplyBlock') TemplateDefaultBlock'
  deriving stock (Show)

data TemplateApplyBlock' = TemplateApplyBlock' Expression' Template'
  deriving stock (Show)

data TemplateDefaultBlock' = TemplateDefaultBlock' Template'
  deriving stock (Show)

data Expression'
  = NameExpression' String
  | StringExpression' String
  | IntExpression' Int
  | DoubleExpression' Double
  | BoolExpression' Bool
  | ApplyExpression' Expression' Expression'
  | AccessExpression' Expression' Expression'
  | FilterExpression' Expression' Expression'
  | ContextExpression' [(String, Expression')]
  deriving stock (Show)

infix 4 `approxEq`

class ApproxEq a b where
  approxEq :: a -> b -> Bool

instance ApproxEq Template Template' where
  Template x _ `approxEq` Template' y = x `approxEq` y

instance ApproxEq Block Block' where
  b `approxEq` b' =
    case (b, b') of
      (TemplateElseBlock _, TemplateElseBlock') -> True
      (TemplateEndBlock _, TemplateEndBlock') -> True
      (TextBlock x _, TextBlock' y) -> x == y
      (CommentBlock x _, CommentBlock' y) -> x == y
      (ExpressionBlock x _, ExpressionBlock' y) ->
        x `approxEq` y
      (LayoutBlock x _, LayoutBlock' y) ->
        x `approxEq` y
      (TemplateStartBlock x _, TemplateStartBlock' y) ->
        x `approxEq` y
      (TemplateNextBlock x _, TemplateNextBlock' y) ->
        x `approxEq` y
      (LayoutApplyBlock x xs _, LayoutApplyBlock' y ys) ->
        x `approxEq` y && xs `approxEq` ys
      (TemplateBlock xs x _, TemplateBlock' ys y) ->
        xs `approxEq` ys && x `approxEq` y
      _ -> False

instance ApproxEq Expression Expression' where
  e `approxEq` e' =
    case (e, e') of
      (NameExpression x _, NameExpression' y) -> x == y
      (StringExpression x _, StringExpression' y) -> x == y
      (IntExpression x _, IntExpression' y) -> x == y
      (DoubleExpression x _, DoubleExpression' y) -> x == y
      (BoolExpression x _, BoolExpression' y) -> x == y
      (ApplyExpression xf xa _, ApplyExpression' yf ya) ->
        xf `approxEq` yf && xa `approxEq` ya
      (AccessExpression xt xf _, AccessExpression' yt yf) ->
        xt `approxEq` yt && xf `approxEq` yf
      (FilterExpression xa xf _, FilterExpression' ya yf) ->
        xa `approxEq` ya && xf `approxEq` yf
      (ContextExpression xs _, ContextExpression' ys) ->
        xs `approxEq` ys
      _ -> False

instance ApproxEq TemplateApplyBlock TemplateApplyBlock' where
  TemplateApplyBlock x xs _ `approxEq` TemplateApplyBlock' y ys =
    x `approxEq` y && xs `approxEq` ys

instance ApproxEq TemplateDefaultBlock TemplateDefaultBlock' where
  TemplateDefaultBlock x _ `approxEq` TemplateDefaultBlock' y =
    x `approxEq` y

instance (ApproxEq a b) => ApproxEq [a] [b] where
  (x : xs) `approxEq` (y : ys) = x `approxEq` y && xs `approxEq` ys
  [] `approxEq` [] = True
  _ `approxEq` _ = False

instance (Eq c, ApproxEq a b) => ApproxEq (c, a) (c, b) where
  (z, x) `approxEq` (z', y) =
    z == z' && x `approxEq` y

instance (ApproxEq a b) => ApproxEq (NonEmpty a) (NonEmpty b) where
  (x :| xs) `approxEq` (y :| ys) =
    x `approxEq` y && xs `approxEq` ys
