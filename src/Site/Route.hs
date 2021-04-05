module Site.Route where

import Site.Common

indexRoute :: Routes
indexRoute = gsubRoute "\\.html$" $ replaceAll "\\.html$" (const "/index.html")
