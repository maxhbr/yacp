{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}
module YACP.Core.Model.Relation
  ( module X
  -- Relations
  , RelationType (..)
  , Relation (..)
  , relationContainsIdentifier
  , normalizeRelation
  , Relations (..)
  , normalizeRelations
  ) where

import YACP.Core.MyPrelude
import YACP.Core.Model.Identifier
import SPDX.Document.RelationshipTypes as X

import System.Console.Pretty (color, Color(Green))
import System.IO (hPutStrLn, stderr)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, maybeToList, catMaybes)
import Data.UUID (UUID)
import qualified Data.Map                      as Map
import System.Random (randomIO)
import Data.String (IsString(..))
import qualified Control.Monad.State as MTL
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Monoid (mconcat)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Distribution.SPDX as SPDX
import qualified Distribution.SPDX.Extra as SPDX
import qualified Distribution.SPDX.License as SPDX
import qualified Distribution.Parsec as SPDX
import qualified Network.URI as URI
import qualified System.FilePath as FP
import Data.Typeable
import Data.Dynamic

data Relation
  = Relation
  { _getRelationSrc :: Identifier
  , _getRelationType :: RelationType
  , _getRelationTarget :: Identifier
  } deriving (Eq, Generic)
instance A.ToJSON Relation
instance A.FromJSON Relation
instance Show Relation where
  show (Relation rSrc rType rTarget) = "{{{" ++ show rSrc ++ " >" ++ show rType ++ "> " ++ show rTarget ++ "}}}"
instance IdentifierProvider Relation where
  getIdentifiers (Relation src _ target) = V.fromList [src, target]

relationContainsIdentifier :: Identifiable a => a -> Relation -> Bool
relationContainsIdentifier a (Relation src _ target) = any (`matchesIdentifiable` a) [src, target]

{-|
  direction should always be from the smaller to the bigger in which it is included
-}
normalizeRelation :: Relation -> Relation
normalizeRelation = let
    flipDirection :: Relation -> Relation
    flipDirection (r@Relation { _getRelationSrc = src
                              , _getRelationTarget = target
                              }) =
      r {_getRelationSrc = target, _getRelationTarget = src}
    normalizeRelation' :: Relation -> Relation
    normalizeRelation' (r@Relation {_getRelationType = DEPENDS_ON}) =
      flipDirection (r {_getRelationType = DEPENDENCY_OF})
    normalizeRelation' (r@Relation {_getRelationType = DESCRIBED_BY}) =
      flipDirection (r {_getRelationType = DESCRIBES})
    normalizeRelation' (r@Relation {_getRelationType = CONTAINS}) =
      flipDirection (r {_getRelationType = CONTAINED_BY})
    normalizeRelation' (r@Relation {_getRelationType = HAS_PREREQUISITE}) =
      flipDirection (r {_getRelationType = PREREQUISITE_FOR})
    normalizeRelation' (r@Relation {_getRelationType = GENERATED_FROM}) =
      flipDirection (r {_getRelationType = GENERATES})
    normalizeRelation' r = r
  in normalizeRelation'

data Relations
  = Relations (Vector Relation)
  deriving (Eq, Show, Generic)
instance A.ToJSON Relations
instance A.FromJSON Relations
instance Semigroup Relations where
    (Relations rs1) <> (Relations rs2) = Relations (vNub (rs1 <> rs2))
instance Monoid Relations where
    mempty = Relations mempty
instance IdentifierProvider Relations where
  getIdentifiers (Relations rs) = V.concatMap getIdentifiers rs

normalizeRelations :: Relations -> Relations
normalizeRelations (Relations rs) = Relations (V.map normalizeRelation rs)