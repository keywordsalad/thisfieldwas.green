module Green.Command where

import Green.Config
import Green.Util
import Options.Applicative

data AuthorCommand
  = CreateDraft CreateDraftOpts
  | PublishPost FilePath
  deriving (Show, Eq)

data CreateDraftOpts = CreateDraftOpts
  { draftTitle :: String,
    draftCategory :: Maybe String
  }
  deriving (Show, Eq)

authorCommands :: String -> ParserInfo AuthorCommand
authorCommands progName = authorOptions
  where
    --- commands
    authorOptions = info (authorOptions' <**> helper) (fullDesc <> authorDesc)
    authorOptions' = subparser draftCommand <|> subparser publishCommand
    authorDesc = progDesc (progName ++ " -- an admin tool for the site")
    --- draft command
    draftCommand = command "draft" $ info draftOptions (progDesc "Create a new draft post")
    draftOptions = CreateDraft <$> draftOptions'
    draftOptions' =
      CreateDraftOpts
        <$> strOption (long "title" <> short 't')
        <*> optional (strOption (long "category" <> short 'c'))
    --- publish command
    publishCommand = command "publish" $ info publishOptions (progDesc "Publish an existing draft")
    publishOptions = PublishPost <$> strOption (long "file" <> short 'f')

processAuthorCommand :: SiteConfig -> AuthorCommand -> IO ()
processAuthorCommand siteConfig (CreateDraft draftOpts) = createDraft siteConfig draftOpts
processAuthorCommand _ (PublishPost file) = putStrLn ("Publishing " ++ file)

createDraft :: SiteConfig -> CreateDraftOpts -> IO ()
createDraft _ (CreateDraftOpts title maybeCategory) =
  putStrLn $ "Writing post '" ++ title ++ "' to file " ++ draftFilePath
  where
    draftFilePath = categoryPrefix ++ camelToKebab title ++ ".md"
    categoryPrefix = maybe "" ((++ "/") . camelToKebab) maybeCategory
