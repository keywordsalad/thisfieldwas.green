module Site.Context.Field where

import Hakyll

includeCodeField :: Context String
includeCodeField = functionField "include-code" f
  where
    f (lexer:contentsPath:[]) _ = fmap wrapCode body
      where
        wrapCode code = "```" ++ lexer ++ "\n" ++ code ++ "\n```"
        body = loadSnapshotBody id' "code"
        id' = fromFilePath $ "code/" ++ contentsPath
    f _ id' = error $ "codeIncludeField needs a filepath and a lexer " ++ show (itemIdentifier id')
