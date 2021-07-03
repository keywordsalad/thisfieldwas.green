module Green.Context.FieldError where

import Green.Common

fieldError :: String -> [String] -> [String] -> Item a -> b
fieldError targetKey argNames argValues item =
  error $ msg ++ " in " ++ show (itemIdentifier item)
  where
    msg = targetKey
      ++ " expected "
      ++ show argNames
      ++ " but received "
      ++ show argValues
