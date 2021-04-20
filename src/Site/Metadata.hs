module Site.Metadata where

import Hakyll

pageType :: Metadata -> Maybe String
pageType = lookupString "page-type"

isStaticPage :: Metadata -> Bool
isStaticPage = (== Just "static") . pageType
