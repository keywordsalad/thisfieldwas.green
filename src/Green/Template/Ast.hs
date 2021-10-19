module Green.Template.Ast
  ( Template (..),
    Block (..),
    getBlockName,
    getBlockPos,
    ApplyBlock (..),
    DefaultBlock (..),
    Expression (..),
    getExpressionPos,
    PrettyPrint (prettyPrint),
  )
where

import Data.Binary
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics
import Green.Common
import Text.Parsec hiding (getPosition)
import Text.Parsec.Pos

data Template = Template [Block] FilePath
  deriving stock (Show, Generic)

instance Binary Template where
  get = Template <$> get <*> get

  put (Template blocks pos) = do
    put blocks
    put pos

instance Writable Template where
  write _ _ = return ()

data Block
  = TextBlock String SourcePos
  | ExpressionBlock Expression SourcePos
  | CommentBlock String SourcePos
  | ChromeBlock Expression [Block] SourcePos
  | AltBlock
      (NonEmpty ApplyBlock)
      (Maybe DefaultBlock)
      SourcePos
  deriving stock (Show, Generic)

getBlockName :: Block -> String
getBlockName = \case
  TextBlock {} -> "TextBlock"
  ExpressionBlock {} -> "ExpressionBlock"
  CommentBlock {} -> "CommentBlock"
  ChromeBlock {} -> "ApplyBlock"
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

newtype BinaryPos = BinaryPos SourcePos
  deriving stock (Show)

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
  deriving stock (Show, Generic)

instance Binary ApplyBlock where
  get = do
    f <- ApplyBlock <$> get <*> get
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put (ApplyBlock expression blocks pos) = do
    put expression
    put blocks
    put $ BinaryPos pos

data DefaultBlock = DefaultBlock [Block] SourcePos
  deriving stock (Show, Generic)

instance Binary DefaultBlock where
  get = do
    f <- DefaultBlock <$> get
    binaryPos <- get :: Get BinaryPos
    return $ f (unBinaryPos binaryPos)

  put (DefaultBlock blocks pos) = do
    put blocks
    put $ BinaryPos pos

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
  deriving stock (Show, Generic)

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

indent :: Int -> String
indent level = mconcat $ replicate level "  "

class PrettyPrint a where
  prettyPrint :: a -> String
  prettyPrint item =
    prettyPrint' 0 item ++ "\n"

  prettyPrint' :: Int -> a -> String
  prettyPrint' level item =
    indent level ++ prettyIndented' (level + 1) item

  prettyLabel' :: Int -> String -> a -> String
  prettyLabel' level label' item =
    indent level ++ label' ++ ": " ++ prettyIndented' (level + 1) item

  prettyIndented' :: Int -> a -> String

instance PrettyPrint SourcePos where
  prettyIndented' _ pos = "in " ++ show pos

instance (PrettyPrint a) => PrettyPrint (String, a) where
  prettyIndented' level (label', item) =
    show label' ++ " -> " ++ prettyIndented' level item

instance (PrettyPrint a) => PrettyPrint (Maybe a) where
  prettyIndented' level = \case
    Just item -> prettyIndented' level item
    Nothing -> "Undefined"

instance (PrettyPrint a) => PrettyPrint [a] where
  prettyIndented' level items
    | null items = "[]"
    | otherwise =
      "[\n"
        ++ (intercalate ",\n" (prettyPrint' (level + 1) <$> items) ++ "\n")
        ++ (indent level ++ "]")

instance PrettyPrint Template where
  prettyIndented' level (Template blocks pos) =
    intercalate "\n" $
      [ "Template in " ++ show pos,
        prettyLabel' level "blocks" blocks
      ]

instance PrettyPrint Block where
  prettyIndented' level = \case
    ExpressionBlock expression pos ->
      intercalate "\n" $
        [ "ExpressionBlock",
          pl "expression" expression,
          pp pos
        ]
    TextBlock text pos ->
      intercalate "\n" $
        [ "TextBlock",
          prettyText text,
          pp pos
        ]
    CommentBlock comment pos ->
      intercalate "\n" $
        [ "CommentBlock",
          prettyText comment,
          pp pos
        ]
    ChromeBlock expression blocks pos ->
      intercalate "\n" $
        [ "ChromeBlock",
          pl "expression" expression,
          pl "blocks" blocks,
          pp pos
        ]
    AltBlock blocks defaultBlock pos ->
      intercalate "\n" $
        [ "AltBlock",
          pl "blocks" (NonEmpty.toList blocks),
          pl "default" defaultBlock,
          pp pos
        ]
    where
      prettyText text = unlines $ (indent level ++) <$> lines text
      pp = prettyPrint' level
      pl :: (PrettyPrint a) => String -> a -> String
      pl = prettyLabel' level

instance PrettyPrint ApplyBlock where
  prettyIndented' level (ApplyBlock expression blocks pos) =
    intercalate "\n" $
      [ "ApplyBlock",
        prettyLabel' level "guard" expression,
        prettyLabel' level "blocks" blocks,
        prettyPrint' level pos
      ]

instance PrettyPrint DefaultBlock where
  prettyIndented' level (DefaultBlock blocks pos) =
    intercalate "\n" $
      [ "DefaultBlock",
        prettyLabel' level "blocks" blocks,
        prettyPrint' level pos
      ]

instance PrettyPrint Expression where
  prettyIndented' level = \case
    NameExpression name pos ->
      intercalate "\n" $
        [ "NameExpression " ++ show name,
          pp pos
        ]
    StringExpression value pos ->
      intercalate "\n" $
        [ "StringExpression " ++ show value,
          pp pos
        ]
    IntExpression value pos ->
      intercalate "\n" $
        [ "IntExpression " ++ show value,
          pp pos
        ]
    DoubleExpression value pos ->
      intercalate "\n" $
        [ "DoubleExpression " ++ show value,
          pp pos
        ]
    BoolExpression value pos ->
      intercalate "\n" $
        [ "BoolExpression " ++ show value,
          pp pos
        ]
    ApplyExpression f x pos ->
      intercalate "\n" $
        [ "ApplyExpression",
          pl "fn" f,
          pl "arg" x,
          pp pos
        ]
    AccessExpression target field pos ->
      intercalate "\n" $
        [ "AccessExpression",
          pl "target" target,
          pl "field" field,
          pp pos
        ]
    FilterExpression x f pos ->
      intercalate "\n" $
        [ "FilterExpression",
          pl "arg" x,
          pl "filter" f,
          pp pos
        ]
    ContextExpression context pos ->
      intercalate "\n" $
        [ "ContextExpression",
          pp context,
          pp pos
        ]
    ListExpression values pos ->
      intercalate "\n" $
        [ "ListExpression",
          pp values,
          pp pos
        ]
    where
      pp :: (PrettyPrint a) => a -> String
      pp = prettyPrint' level
      pl = prettyLabel' level
