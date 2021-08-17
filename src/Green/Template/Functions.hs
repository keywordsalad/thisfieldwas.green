module Green.Template.Functions where

import Green.Template.Compiler
import Green.Template.Context
import qualified Hakyll as H

includeField :: Context String
includeField = constField "include" f
  where
    f :: FunctionValue String String String
    f id' context _ = do
      item <- H.loadBody $ H.fromFilePath id'
      templated <- applyAsTemplate context item
      return $ H.itemBody templated

-- withField :: forall a. Context a
-- withField = constField "with" f
--   where
--     f :: FunctionValue2 a
--     f contextArg wrapArg context item = do
