module Green.Template.Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isDigit, isLower, isUpper)
import Data.Functor
import Data.List.NonEmpty as NEL hiding (reverse)
import Data.Scientific hiding (scientific)
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

parseTemplate :: String -> String -> Either String Template
parseTemplate origin = parseOnly (template origin <* endOfInput) . T.pack

template :: String -> Parser Template
template origin = Template origin <$> blocks origin

blocks :: String -> Parser [Block]
blocks origin = many (block origin)

block :: String -> Parser Block
block origin =
  commentBlock
    <|> templateBlock origin
    <|> expressionBlock
    <|> textBlock

commentBlock :: Parser Block
commentBlock = CommentBlock <$> (openWith "!" *> text <* close)

templateBlock :: String -> Parser Block
templateBlock origin = do
  startTemplate' <- startTemplate
  altTemplates' <- many altTemplate
  defaultTemplate' <- optional defaultTemplate
  endTemplate
  return $ TemplateBlock (startTemplate' :| altTemplates') defaultTemplate'
  where
    templateTag = "#"
    openTemplate = openWith templateTag
    startTemplate = do
      guard <- openTemplate *> expression <* close
      template' <- template origin
      return (guard, template')
    altTemplate = do
      guard <- openTemplate *> keyword "else" *> expression <* close
      template' <- template origin
      return (guard, template')
    defaultTemplate = openTemplate *> keyword "else" *> close *> template origin
    endTemplate = endWith templateTag

textBlock :: Parser Block
textBlock = TextBlock <$> text

text :: Parser String
text = many $ char' <|> escapeChar'
  where
    char' = satisfy (notInClass "{}\\")
    escapeChar' = char '\\' *> satisfy (inClass "{}\\")

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
    <$> accessExpression `sepBy1` spaces

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
  subexpression
    <|> boolExpression
    <|> nameExpression
    <|> stringExpression
    <|> scientificExpression

nameExpression :: Parser Expression
nameExpression = NameExpression <$> name

stringExpression :: Parser Expression
stringExpression = do
  dquote $> ()
  value <- many (normalChar <|> escapeChar)
  dquote $> ()
  return $ StringExpression value
  where
    dquote = char '"'
    normalChar = satisfy (notInClass "\\\n\"")
    escapeChar = char '\\' *> escapeChar'
    escapeChar' =
      char 'n' $> '\n'
        <|> char 't' $> '\t'
        <|> char '\\' $> '\\'
        <|> char '"' $> '"'
        <|> char '\'' $> '\''

scientificExpression :: Parser Expression
scientificExpression = convert =<< scientific
  where
    convert n
      | isInteger n =
        maybe
          (fail $ "Could not convert scientific " ++ show n ++ " to int")
          (return . IntExpression)
          (toBoundedInteger n)
      | isFloating n =
        either
          (\_ -> fail $ "Could not convert scientific " ++ show n ++ " to double")
          (return . DoubleExpression)
          (toBoundedRealFloat n)
      | otherwise = fail $ "Unhandled scientific " ++ show n

boolExpression :: Parser Expression
boolExpression =
  BoolExpression <$> (true <|> false)
  where
    true = keyword "true" $> True
    false = keyword "false" $> False

subexpression :: Parser Expression
subexpression = keyword "(" *> expression <* keyword ")"

contextExpression :: Parser Expression
contextExpression = ContextExpression <$> (keyword "{" *> keyValues <* keyword "}")
  where
    comma = keyword ","
    keyValues = do
      pairs <- keyValue `sepBy` comma
      comma <|> pure ()
      return pairs
    keyValue = do
      key <- name
      keyword ":"
      value <- expression
      return (key, value)

name :: Parser String
name = (:) <$> nameStart <*> many nameRest <?> "name"
  where
    nameStart = satisfy isLower <|> char '_'
    nameRest = nameStart <|> satisfy isUpper <|> satisfy isDigit <|> char '-'

spaces :: Parser ()
spaces = many1 space $> ()

keyword :: String -> Parser ()
keyword x = skipSpace *> string (T.pack x) *> skipSpace <?> "keyword " ++ show x

open :: Parser ()
open = openWith ""

close :: Parser ()
close = keyword "}}"

openWith :: String -> Parser ()
openWith tag = keyword ("{{" ++ tag)

endWith :: String -> Parser ()
endWith tag = openWith tag *> keyword "end" *> close
