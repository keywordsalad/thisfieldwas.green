module Green.Hakyllbars.Field.Html
  ( escapeHtmlField,
    escapeHtmlUriField,
    escapeJsonField,
  )
where

import Data.Char (ord)
import Green.Hakyllbars.Common
import Green.Hakyllbars.Context
import Network.URI (escapeURIString, isUnescapedInURI)
import Numeric (showHex)

-- | Escapes HTML before interpolating in a template.
escapeHtmlField :: Context String
escapeHtmlField = functionField "escapeHtml" f
  where
    f = return . escapeHtml

-- | Escapes HTML URI before interpolating in a template.
escapeHtmlUriField :: Context String
escapeHtmlUriField = functionField "escapeHtmlUri" f
  where
    f = return . escapeHtml . escapeURIString isUnescapedInURI

-- | Escapes a string for safe interpolation into a JSON string literal, e.g.
-- inside a @\<script type="application/ld+json"\>@ block.
escapeJsonField :: Context String
escapeJsonField = functionField "escapeJson" f
  where
    f (s :: String) = return (concatMap escapeJsonChar s)

-- | Escapes a single character for a JSON string literal. The @\<@ escape guards
-- against a @\</script\>@ sequence breaking out of an inline JSON-LD block.
escapeJsonChar :: Char -> String
escapeJsonChar c = case c of
  '\\' -> "\\\\"
  '"' -> "\\\""
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '<' -> "\\u003c"
  _
    | ord c < 0x20 -> "\\u" ++ pad (showHex (ord c) "")
    | otherwise -> [c]
  where
    pad s = replicate (4 - length s) '0' ++ s
