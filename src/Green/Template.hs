module Green.Template where

import qualified Data.Text as T

newtype Template = Template [Block]

data Block
  = TextBlock T.Text
  | ExpressionBlock Expression

data Expression
  = VarExpression T.Text
  | CallExpresion T.Text [Expression]
  | FilterExpression Expression Expression
  | StringExpression T.Text
  | IntegerExpression Integer
  | DoubleExpression Double
  | BoolExpression Bool
