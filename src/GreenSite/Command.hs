module GreenSite.Command where

import GreenSite.Util
import Options.Applicative

data GreenCommand
  = CreateDraft CreateDraftOpts
  | PublishPost FilePath
  deriving stock (Show, Eq)

data CreateDraftOpts = CreateDraftOpts
  { draftTitle :: String,
    draftCategory :: Maybe String
  }
  deriving stock (Show, Eq)

greenCommands :: String -> ParserInfo GreenCommand
greenCommands progName = greenOptions
  where
    --- commands
    greenOptions = info (greenOptions' <**> helper) (fullDesc <> greenDesc)
    greenOptions' = subparser draftCommand <|> subparser publishCommand
    greenDesc = progDesc (progName ++ " -- an admin tool for the site")
    --- draft command
    draftCommand = command "draft" $ info (draftOptions <**> helper) (progDesc "Create a new draft post")
    draftOptions = CreateDraft <$> (CreateDraftOpts <$> strOption (long "title" <> short 't') <*> optional (strOption (long "category" <> short 'c')))
    --- publish command
    publishCommand = command "publish" $ info (publishOptions <**> helper) (progDesc "Publish an existing draft")
    publishOptions = PublishPost <$> strOption (long "file" <> short 'f')

processGreenCommand :: GreenCommand -> IO ()
processGreenCommand (CreateDraft draftOpts) = createDraft draftOpts
processGreenCommand (PublishPost file) = putStrLn ("Publishing " ++ file)

createDraft :: CreateDraftOpts -> IO ()
createDraft (CreateDraftOpts title maybeCategory) =
  putStrLn $ "Writing post '" ++ title ++ "' to file " ++ draftFilePath
  where
    draftFilePath = categoryPrefix ++ kebabCase title ++ ".md"
    categoryPrefix = maybe "" ((++ "/") . kebabCase) maybeCategory
