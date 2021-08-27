module Green.Template.Custom.Compiler where

import Green.Common
import Green.Config
import Green.Template
import Green.Template.Custom.Context
import qualified Hakyll as H
import System.FilePath

pageCompiler :: SiteConfig -> Compiler (Item String)
pageCompiler config =
  H.getResourceBody
    >>= applyAsTemplate context
    >>= compilePandoc
    >>= \item -> do
      metadataContext <- getContext $ H.itemIdentifier item
      unContext metadataContext "layout" item >>= \case
        StringValue layoutName -> do
          let layoutPath = "_layouts" </> layoutName <.> "html"
          layoutTemplate <- loadTemplateBody $ H.fromFilePath layoutPath
          applyTemplate layoutTemplate context item
        _ -> do
          H.debugCompiler $ "Did not receive String layout key for " ++ show (H.itemIdentifier item)
          return item
  where
    context = customContext config
