module Green.Context
  ( module Green.Context.Field,
    module Green.Context.GitCommits,
    baseContext,
  )
where

import Data.Time
import Green.Config
import Green.Context.Field
import Green.Context.GitCommits
import Hakyll hiding (dateField)
import Lens.Micro

baseContext :: SiteConfig -> Context String
baseContext config = do
  let context =
        mconcat
          [ constField "linkedinProfile" (config ^. siteLinkedInProfile),
            trimIndexUrlField "url",
            gitCommits (config ^. siteGitWebUrl),
            imgField,
            youtubeField,
            getRouteField,
            commentField,
            siteRootField (config ^. siteRoot),
            defaultContext,
            constField "body-class" "default",
            constField "contactEmail" (config ^. siteAuthorEmail),
            datesContext (config ^. siteTimeLocale)
          ]
      dependentContexts =
        [ getCodeField,
          linkedTitleField
        ]
   in mconcat (dependentContexts <*> pure context) <> context

datesContext :: TimeLocale -> Context String
datesContext timeLocale =
  mconcat $
    flip (dateField timeLocale) format
      <$> ["date", "published", "updated"]
  where
    format = "%B %e, %Y"
