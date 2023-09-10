module Green.Template.Compiler where

import Data.Text qualified as T
import Green.Common
import Hakyll
import System.FilePath
import Text.Pandoc

calculateReadingTimeCompiler :: Item String -> Compiler (Item String)
calculateReadingTimeCompiler item@(Item id' _) = do
  let ext = takeExtension $ toFilePath id'
  go ext
  where
    go ".html" = return item
    go _ = renderPandocWithTransform readerOptions writerOptions calculateReadingTime item

readerOptions :: ReaderOptions
readerOptions =
  defaultHakyllReaderOptions
    { readerExtensions =
        foldl (flip ($)) (readerExtensions defaultHakyllReaderOptions) $
          [ enableExtension Ext_smart,
            enableExtension Ext_inline_code_attributes,
            disableExtension Ext_markdown_in_html_blocks
          ]
    }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

calculateReadingTime :: Pandoc -> Pandoc
calculateReadingTime doc@(Pandoc meta blocks) =
  Pandoc meta (timeEstimateBlock : blocks)
  where
    timeEstimateBlock =
      Div
        ("", ["estimated-reading-time"], [])
        [ Para
            [ Str "Estimated reading time: ",
              Span ("", ["length"], []) [Str timeEstimate]
            ]
        ]
    timeEstimate = timeEstimateString doc
    timeEstimateString = T.pack . toClockString . timeEstimateSeconds
    timeEstimateSeconds = (`quot` wordsPerSecond) . wordsLength
    wordsLength = (`quot` lettersPerWord) . documentLength
    wordsPerSecond = 4
    lettersPerWord = 5

toClockString :: Int -> String
toClockString i
  | i >= 60 * 60 = show hours ++ "h " ++ show minutes ++ "m " ++ show seconds ++ "s"
  | i >= 60 = show minutes ++ "m " ++ show seconds ++ "s"
  | otherwise = show seconds ++ "s"
  where
    hours = i `quot` (60 * 60)
    minutes = (i `rem` (60 * 60)) `quot` 60
    seconds = i `rem` 60

documentLength :: Pandoc -> Int
documentLength (Pandoc _ blocks') = blocksLength blocks'
  where
    blocksLength = sum . (blockLength <$>)
    blockLength = \case
      Plain inlines -> inlinesLength inlines
      Para inlines -> inlinesLength inlines
      LineBlock inlinesList -> sum (inlinesLength <$> inlinesList)
      CodeBlock _ text -> T.length text
      RawBlock _ text -> T.length text
      BlockQuote blocks -> blocksLength blocks
      OrderedList _ blocksList -> blocksListLength blocksList
      BulletList blocksList -> blocksListLength blocksList
      DefinitionList x0 -> sum $ uncurry (+) . bimap inlinesLength blocksListLength <$> x0
      Header _ _ inlines -> inlinesLength inlines
      HorizontalRule -> 1
      Table _ cap _ th tbs tf -> captionLength cap + lettersFromHeader th + sum (tableLength <$> tbs) + lettersFromFooter tf
        where
          tableLength (TableBody _ _ rows1 rows2) = sum (lettersFromRow <$> rows1) + sum (lettersFromRow <$> rows2)
          lettersFromHeader (TableHead _ rows) = sum (lettersFromRow <$> rows)
          lettersFromRow (Row _ cells) = sum (lettersFromCell <$> cells)
          lettersFromCell (Cell _ _ _ _ blocks) = sum (blockLength <$> blocks)
          lettersFromFooter (TableFoot _ rows) = sum (lettersFromRow <$> rows)
      Div _ blocks -> blocksLength blocks
      Figure _ caption blocks -> captionLength caption + blocksLength blocks
      where
        blocksListLength blocksList = sum (blocksLength <$> blocksList)
        captionLength (Caption inlines blocks) = maybe 0 inlinesLength inlines + blocksLength blocks

    inlinesLength = sum . (inlineLength <$>)
      where
        inlineLength = \case
          Str text -> T.length text
          Emph inlines -> inlinesLength inlines
          Underline inlines -> inlinesLength inlines
          Strong inlines -> inlinesLength inlines
          Strikeout inlines -> inlinesLength inlines
          Superscript inlines -> inlinesLength inlines
          Subscript inlines -> inlinesLength inlines
          SmallCaps inlines -> inlinesLength inlines
          Quoted _ inlines -> inlinesLength inlines
          Cite cis inlines -> sum (lettersFromCitation <$> cis) + inlinesLength inlines
          Code _ text -> T.length text
          Space -> 1
          SoftBreak -> 1
          LineBreak -> 1
          Math _ text -> T.length text
          RawInline _ text -> T.length text
          Link _ inlines _ -> inlinesLength inlines
          Image _ inlines _ -> inlinesLength inlines
          Note blocks -> blocksLength blocks
          Span _ inlines -> inlinesLength inlines
        lettersFromCitation citation = inlinesLength (citationPrefix citation) + inlinesLength (citationSuffix citation)
