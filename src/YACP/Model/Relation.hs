{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeFamilies              #-}

module YACP.Model.Relation
  ( module X
  -- Relations
  , RelationType(..)
  , Relation(..)
  , relationContainsIdentifier
  ) where

import           YACP.Model.Identifier
import           YACP.YacpPrelude

import qualified PURL.PURL                       as PURL
import           SPDX.Document.RelationshipTypes as X

import qualified Control.Monad.State             as MTL
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Types                as A
import           Data.List                       (nub)
import           Data.List.Split                 (splitOn)
import           Data.Maybe                      (fromMaybe, maybeToList)
import qualified Data.Monoid                     (mconcat)
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import           Data.UUID                       (UUID)
import qualified Data.Vector                     as V
import qualified Distribution.Parsec             as SPDX
import qualified Distribution.SPDX               as SPDX
import qualified Distribution.SPDX.Extra         as SPDX
import qualified SPDX.Document.Common            as SPDX
import           System.Console.Pretty           (Color (Green), color)
import qualified System.FilePath                 as FP
import           System.IO                       (hPutStrLn, stderr)
import           System.Random                   (randomIO)

data Relation =
  Relation
    { _getRelationSrc    :: Identifier
    , _getRelationType   :: RelationType
    , _getRelationTarget :: Identifier
    }
  deriving (Eq, Generic)

instance A.ToJSON Relation

instance A.FromJSON Relation

instance Show Relation where
  show (Relation rSrc rType rTarget) =
    "{{{" ++ show rSrc ++ " >" ++ show rType ++ "> " ++ show rTarget ++ "}}}"

relationContainsIdentifier :: Identifiable a => a -> Relation -> Bool
relationContainsIdentifier a (Relation src _ target) =
  any (`matchesIdentifiable` a) [src, target]
{-|
  direction should always be from the smaller to the bigger in which it is included
-}
