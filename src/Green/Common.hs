module Green.Common
  ( module Control.Applicative,
    module Control.Exception,
    module Control.Monad,
    module Control.Monad.Except,
    module Control.Monad.Trans,
    module Data.Bifunctor,
    module Data.Bool,
    module Data.Foldable,
    module Data.Functor,
    module Data.List,
    module Data.Maybe,
    module Data.Time,
    module Data.Time.Format,
    module Hakyll,
    module Lens.Micro,
    module Lens.Micro.TH,
    module System.Directory,
    module System.FilePath,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad (forM, join, (<=<), (>=>))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap, first, second)
import Data.Bool (bool)
import Data.Foldable (sequenceA_)
import Data.Functor ((<&>))
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, maybe, maybeToList)
import Data.Time (ZonedTime)
import Data.Time.Format (TimeLocale, formatTime, parseTime, parseTimeM)
import Hakyll
  ( -- types
    Compiler,
    Dependency,
    FeedConfiguration (..),
    Identifier (..),
    Item (..),
    Metadata,
    Pattern,
    Redirect (..),
    Routes,
    Rules,
    Snapshot,
    -- typeclasses
    Writable (..),
    -- functions
    cached,
    compile,
    composeRoutes,
    copyFileCompiler,
    create,
    debugCompiler,
    escapeHtml,
    fromFilePath,
    fromList,
    fromRegex,
    getMatches,
    getMetadata,
    getResourceBody,
    getResourceString,
    getRoute,
    gsubRoute,
    idRoute,
    itemSetBody,
    load,
    loadSnapshot,
    loadSnapshotBody,
    lookupString,
    makeItem,
    makePatternDependency,
    match,
    matchRoute,
    noResult,
    relativizeUrls,
    route,
    rulesExtraDependencies,
    saveSnapshot,
    setExtension,
    toFilePath,
    toUrl,
    unsafeCompiler,
    withErrorMessage,
    withItemBody,
  )
import Lens.Micro hiding ((<&>))
import Lens.Micro.TH
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath (dropExtension, splitDirectories, splitFileName, takeDirectory, takeFileName, (</>))
