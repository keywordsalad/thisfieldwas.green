module Green.Hakyllbars.Common
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
    module System.Directory,
    module System.FilePath,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad (forM, join, void, (<=<), (>=>))
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap, first, second)
import Data.Bool (bool)
import Data.Foldable (sequenceA_)
import Data.Functor ((<&>))
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, maybe, maybeToList)
import Data.Time (ZonedTime)
import Data.Time.Format (TimeLocale, formatTime, parseTimeM)
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
    constRoute,
    copyFileCompiler,
    create,
    customRoute,
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
    hasNoVersion,
    hasVersion,
    idRoute,
    itemSetBody,
    load,
    loadBody,
    loadSnapshot,
    loadSnapshotBody,
    lookupString,
    makeItem,
    makePatternDependency,
    match,
    matchRoute,
    metadataRoute,
    noResult,
    relativizeUrls,
    route,
    rulesExtraDependencies,
    saveSnapshot,
    setExtension,
    toFilePath,
    toUrl,
    unsafeCompiler,
    version,
    withErrorMessage,
    withItemBody,
    (.&&.),
    (.||.),
  )
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath (dropExtension, splitDirectories, splitFileName, takeDirectory, takeFileName, (</>))
