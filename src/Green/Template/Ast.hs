module Green.Template.Ast
  ( Template (..),
    Block (..),
    getBlockName,
    getBlockPos,
    ApplyBlock (..),
    DefaultBlock (..),
    Expression (..),
    getExpressionPos,
  )
where

import Data.Binary
import qualified Data.ByteString.Char8 as Char8
import Data.List.NonEmpty (NonEmpty)
import Data.Yaml as Y (ToJSON (..), object, (.=))
import Data.Yaml.Pretty (defConfig, encodePretty)
import GHC.Generics (Generic)
import Green.Common
import Text.Parsec hiding (getPosition)
import Text.Parsec.Pos

showJSON :: (ToJSON a) => a -> String
showJSON = Char8.unpack . encodePretty defConfig . toJSON

data Template = Template [Block] FilePath
  deriving (Generic)

instance Binary Template where
  get = Template <$> get <*> get

  put (Template blocks pos) = do
    put blocks
    put pos

instance Writable Template where
  write _ _ = return ()

instance ToJSON Template

instance Show Template where
  show = showJSON

data Block
  = TextBlock String SourcePos
  | ExpressionBlock Expression SourcePos
  | CommentBlock String SourcePos
  | ChromeBlock Expression [Block] SourcePos
  | AltBlock
      (NonEmpty ApplyBlock)
      (Maybe DefaultBlock)
      SourcePos
  deriving (Generic)

getBlockName :: Block -> String
getBlockName = \case
  TextBlock {} -> "TextBlock"
  ExpressionBlock {} -> "ExpressionBlock"
  CommentBlock {} -> "CommentBlock"
  ChromeBlock {} -> "ChromeBlock"
  AltBlock {} -> "AltBlock"

getBlockPos :: Block -> SourcePos
getBlockPos = \case
  TextBlock _ pos -> pos
  ExpressionBlock _ pos -> pos
  CommentBlock _ pos -> pos
  ChromeBlock _ _ pos -> pos
  AltBlock _ _ pos -> pos

getBlockTag :: Block -> Int
getBlockTag = \case
  TextBlock {} -> 1
  ExpressionBlock {} -> 2
  CommentBlock {} -> 3
  ChromeBlock {} -> 4
  AltBlock {} -> 5

instance Binary Block where
  get = do
    tag <- get :: Get Int
    f <- case tag of
      1 -> TextBlock <$> get
      2 -> ExpressionBlock <$> get
      3 -> CommentBlock <$> get
      4 -> ChromeBlock <$> get <*> get
      5 -> AltBlock <$> get <*> get
      _ -> error $ "Unrecognized block tag " ++ show tag
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put block = do
    put $ getBlockTag block
    case block of
      TextBlock text _ -> put text
      ExpressionBlock expression _ -> put expression
      CommentBlock text _ -> put text
      ChromeBlock expression blocks _ -> put expression >> put blocks
      AltBlock blocks default' _ -> put blocks >> put default'
    put $ BinaryPos (getBlockPos block)

instance ToJSON Block where
  toJSON = \case
    TextBlock text pos ->
      object
        [ "type" .= ("TextBlock" :: String),
          "text" .= text,
          "pos" .= show pos
        ]
    ExpressionBlock expression pos ->
      object
        [ "type" .= ("ExpressionBlock" :: String),
          "expression" .= expression,
          "pos" .= show pos
        ]
    CommentBlock text pos ->
      object
        [ "type" .= ("CommentBlock" :: String),
          "text" .= text,
          "pos" .= show pos
        ]
    ChromeBlock expression blocks _ ->
      object
        [ "type" .= ("ChromeBlock" :: String),
          "expression" .= expression,
          "blocks" .= blocks
        ]
    AltBlock blocks default' _ ->
      object
        [ "type" .= ("AltBlock" :: String),
          "blocks" .= blocks,
          "default" .= default'
        ]

instance Show Block where
  show = showJSON

newtype BinaryPos = BinaryPos SourcePos
  deriving (Show)

unBinaryPos :: BinaryPos -> SourcePos
unBinaryPos (BinaryPos pos) = pos

instance Binary BinaryPos where
  get = BinaryPos <$> (newPos <$> get <*> get <*> get)
  put binaryPos = do
    let pos = unBinaryPos binaryPos
    put $ sourceName pos
    put $ sourceLine pos
    put $ sourceColumn pos

data ApplyBlock = ApplyBlock Expression [Block] SourcePos
  deriving (Generic)

instance Binary ApplyBlock where
  get = do
    f <- ApplyBlock <$> get <*> get
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put (ApplyBlock expression blocks pos) = do
    put expression
    put blocks
    put $ BinaryPos pos

instance ToJSON ApplyBlock where
  toJSON (ApplyBlock expression blocks pos) =
    object
      [ "type" .= ("ApplyBlock" :: String),
        "expression" .= expression,
        "blocks" .= blocks,
        "pos" .= show pos
      ]

instance Show ApplyBlock where
  show = showJSON

data DefaultBlock = DefaultBlock [Block] SourcePos
  deriving (Generic)

instance Binary DefaultBlock where
  get = do
    f <- DefaultBlock <$> get
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put (DefaultBlock blocks pos) = do
    put blocks
    put $ BinaryPos pos

instance ToJSON DefaultBlock where
  toJSON (DefaultBlock blocks pos) =
    object
      [ "type" .= ("DefaultBlock" :: String),
        "blocks" .= blocks,
        "pos" .= show pos
      ]

instance Show DefaultBlock where
  show = showJSON

data Expression
  = NameExpression String SourcePos -- name, id
  | StringExpression String SourcePos -- "a string of text"
  | IntExpression Int SourcePos -- 123, 456, 2
  | DoubleExpression Double SourcePos -- 0.2, 1.45
  | BoolExpression Bool SourcePos -- true, false
  | ApplyExpression Expression Expression SourcePos -- fn arg
  | AccessExpression Expression Expression SourcePos -- target.field
  | FilterExpression Expression Expression SourcePos -- arg | fn
  | ContextExpression [(String, Expression)] SourcePos -- { name0: value0, name1: value1 }
  | ListExpression [Expression] SourcePos -- [a, b, c]
  deriving (Generic)

getExpressionPos :: Expression -> SourcePos
getExpressionPos = \case
  NameExpression _ pos -> pos
  StringExpression _ pos -> pos
  IntExpression _ pos -> pos
  DoubleExpression _ pos -> pos
  BoolExpression _ pos -> pos
  ApplyExpression _ _ pos -> pos
  AccessExpression _ _ pos -> pos
  FilterExpression _ _ pos -> pos
  ContextExpression _ pos -> pos
  ListExpression _ pos -> pos

getExpressionTag :: Expression -> Int
getExpressionTag = \case
  NameExpression {} -> 1
  StringExpression {} -> 2
  IntExpression {} -> 3
  DoubleExpression {} -> 4
  BoolExpression {} -> 5
  ApplyExpression {} -> 6
  AccessExpression {} -> 7
  FilterExpression {} -> 8
  ContextExpression {} -> 9
  ListExpression {} -> 10

instance Binary Expression where
  get = do
    tag <- get :: Get Int
    f <- case tag of
      1 -> NameExpression <$> get
      2 -> StringExpression <$> get
      3 -> IntExpression <$> get
      4 -> DoubleExpression <$> get
      5 -> BoolExpression <$> get
      6 -> ApplyExpression <$> get <*> get
      7 -> AccessExpression <$> get <*> get
      8 -> FilterExpression <$> get <*> get
      9 -> ContextExpression <$> get
      10 -> ListExpression <$> get
      _ -> error $ "Unrecognized expression tag " ++ show tag
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put expression = do
    put $ getExpressionTag expression
    case expression of
      NameExpression value _ -> put value
      StringExpression value _ -> put value
      IntExpression value _ -> put value
      DoubleExpression value _ -> put value
      BoolExpression value _ -> put value
      ApplyExpression fn arg _ -> put fn >> put arg
      AccessExpression target field _ -> put target >> put field
      FilterExpression arg fn _ -> put arg >> put fn
      ContextExpression pairs _ -> put pairs
      ListExpression values _ -> put values
    put $ BinaryPos (getExpressionPos expression)

instance ToJSON Expression where
  toJSON = \case
    NameExpression value pos ->
      object
        [ "type" .= ("NameExpression" :: String),
          "value" .= value,
          "pos" .= show pos
        ]
    StringExpression value pos ->
      object
        [ "type" .= ("StringExpression" :: String),
          "value" .= value,
          "pos" .= show pos
        ]
    IntExpression value pos ->
      object
        [ "type" .= ("IntExpression" :: String),
          "value" .= value,
          "pos" .= show pos
        ]
    DoubleExpression value pos ->
      object
        [ "type" .= ("DoubleExpression" :: String),
          "value" .= value,
          "pos" .= show pos
        ]
    BoolExpression value pos ->
      object
        [ "type" .= ("BoolExpression" :: String),
          "value" .= value,
          "pos" .= show pos
        ]
    ApplyExpression fn arg pos ->
      object
        [ "type" .= ("ApplyExpression" :: String),
          "fn" .= fn,
          "arg" .= arg,
          "pos" .= show pos
        ]
    AccessExpression target field pos ->
      object
        [ "type" .= ("AccessExpression" :: String),
          "target" .= target,
          "field" .= field,
          "pos" .= show pos
        ]
    FilterExpression arg fn pos ->
      object
        [ "type" .= ("FilterExpression" :: String),
          "arg" .= arg,
          "fn" .= fn,
          "pos" .= show pos
        ]
    ContextExpression pairs pos ->
      object
        [ "type" .= ("ContextExpression" :: String),
          "pairs" .= pairs,
          "pos" .= show pos
        ]
    ListExpression values pos ->
      object
        [ "type" .= ("ListExpression" :: String),
          "values" .= values,
          "pos" .= show pos
        ]

instance Show Expression where
  show = showJSON
