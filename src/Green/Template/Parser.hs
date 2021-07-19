module Green.Template.Parser where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Char (isAlpha, isDigit)
import Data.Functor
import Data.List.NonEmpty as NEL hiding (reverse)
import qualified Data.Text as T
import Green.Template.Data
import Prelude hiding (takeWhile)

{-
{{! This is a comment, it won't be rendered }}

{{! This expression is interpolated in-place: }}
{{ "this expression" }}

{{! This expression is piped through a filter: }}
{{ published | formateDate "%Y-%m-%d" }}

{{! Piping is just a fancy way of applying arguments forward: }}
{{ formateDate "%Y-%m-%d" published }}

{{! File inclusion is just a function call }}
{{ include "this/file.md" }}

{{! Contexts may be passed as arguments }}
{{ imageWith { title: "lolcat", src: "kitten.jpg" } }}
{{ "this/file.md" | includeWith { with: this, context: stuff } }}

{{# thisExpression.runs | using thisBody | as an argument }}
  This content is passed as a template to the function
  resolved by the above expression. The function may
  do with the template as it pleases, producing zero of
  more instances of the template output.

  {{# if expression }}
    An if-statement is simply a function that renders the
    body if the expression is truthy.
  {{# else }}
    The else-case is optional, and will be shown if the
    expression above is falsy.
  {{# end }}

  {{# each expression }}
    An each-statement is simply a function that renders the
    body for each item in the expression, using it as the
    context for the body.
  {{# else }}
    The else-case is optional, and will be shown if there
    are no items in the expression above.
  {{# end }}

  {{# if expression }}
    Sometimes you want more than one case...
  {{# else each expression }}
    For example, this will show if the above expression is
    falsy and this expression has items.
  {{# else }}
    And this will be shown if the above two cases are falsy.
    Any number of cases may be chained together, however feels
    appropriate. The keywords `else` and `end` are reserved
    here, but everything else is free to use!
  {{# end }}
{{# end }}
-}

parseTemplate :: String -> Either String Template
parseTemplate = parseOnly (template <* endOfInput) . T.pack

template :: Parser Template
template = Template <$> blocks

blocks :: Parser [Block]
blocks = many block

block :: Parser Block
block =
  commentBlock
    <|> templateBlock
    <|> expressionBlock
    <|> textBlock

templateBlock :: Parser Block
templateBlock = do
  (startGuard, startTemplate') <- startTemplate
  altTemplates <- many altTemplate
  defaultTemplate' <- option (Template []) defaultTemplate
  endTemplate
  return $ TemplateBlock (guard, template') elseCases else''
  where
    openTemplate = openWith "#"
    startTemplate = do
      guard <- openTemplate *> expression <* close
      template' <- template
      return (guard, template')
    altTemplate = do
      guard <- openTemplate *> keyword "else" *> expression <* close
      template' <- template
      return (guard, template')
    defaultTemplate = openTemplate *> keyword "else" *> close *> template
    endTemplate = endWith "#"

textBlock :: Parser Block
textBlock = TextBlock <$> text

text :: Parser String
text = many $ char' <|> escapeChar'
  where
    char' = satisfy (notInClass "{}\\")
    escapeChar' = skip (== '\\') *> satisfy (inClass "{}\\")

commentBlock :: Parser Block
commentBlock = CommentBlock <$> (openWith "!" *> text <* close)

expressionBlock :: Parser Block
expressionBlock = ExpressionBlock <$> (open *> expression <* close)

expression :: Parser Expression
expression = filterExpression

filterExpression :: Parser Expression
filterExpression =
  applyOperands (flip ApplyExpression) . NEL.fromList
    <$> applyExpression `sepBy1` keyword "|"

applyExpression :: Parser Expression
applyExpression =
  applyOperands ApplyExpression . NEL.fromList
    <$> simpleExpression `sepBy1` spaces

accessExpression :: Parser Expression
accessExpression =
  applyOperands AccessExpression . NEL.fromList
    <$> simpleExpression `sepBy1` keyword "."

applyOperands :: (Expression -> Expression -> Expression) -> NonEmpty Expression -> Expression
applyOperands apply (start :| rest) = go start rest
  where
    go fn (arg : args) = go (apply fn arg) args
    go done [] = done

simpleExpression :: Parser Expression
simpleExpression =
  nameExpression
    <|> stringExpression
    <|> doubleExpression
    <|> intExpression
    <|> boolExpression
    <|> subexpression

nameExpression :: Parser Expression
nameExpression = NameExpression <$> name

stringExpression :: Parser Expression
stringExpression = do
  dquote
  value <- many (normalChar <|> escapeChar)
  dquote
  return $ StringExpression value
  where
    dquote = skip (== '"')
    normalChar = satisfy (notInClass "\\\n\"")
    escapeChar = skip (== '\\') *> escapeChar'
    escapeChar' =
      char 'n' $> '\n'
        <|> char 't' $> '\t'
        <|> char '\\' $> '\\'
        <|> char '"' $> '"'
        <|> char '\'' $> '\''

doubleExpression :: Parser Expression
doubleExpression = DoubleExpression <$> signed double

intExpression :: Parser Expression
intExpression = IntExpression <$> signed decimal

boolExpression :: Parser Expression
boolExpression =
  BoolExpression <$> (true <|> false)
  where
    true = "true" $> True
    false = "false" $> False

subexpression :: Parser Expression
subexpression = keyword "(" *> expression <* keyword ")"

contextExpression :: Parser Expression
contextExpression = ContextExpression <$> (keyword "{" *> keyValues <* keyword "}")
  where
    keyValues = keyValue `sepBy` keyword ","
    keyValue = do
      key <- name <* skipSpace
      keyword ":"
      value <- expression <* skipSpace
      return (key, value)

name :: Parser String
name = (:) <$> satisfy isNameStart <*> many (satisfy isNameRest)
  where
    isNameStart c = isAlpha c || c == '_'
    isNameRest c = isNameStart c || isDigit c || c == '-'

spaces :: Parser ()
spaces = many1 space $> ()

braces :: String
braces = "{}"

keyword :: String -> Parser ()
keyword x = skipSpace *> string (T.pack x) *> skipSpace

open :: Parser ()
open = "{{" *> skipSpace

openWith :: String -> Parser ()
openWith prefix = "{{" *> string (T.pack prefix) *> skipSpace

close :: Parser ()
close = skipSpace *> "}}" $> ()

endWith :: String -> Parser ()
endWith prefix = openWith prefix *> keyword "end" *> close
