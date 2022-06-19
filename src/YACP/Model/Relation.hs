{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Model.Relation
  ( module X
  -- Relations
  , RelationType (..)
  , Relation (..)
  , relationContainsIdentifier
  , Relations (..)
  ) where

import YACP.MyPrelude
import SPDX.Document.RelationshipTypes as X
import YACP.Model.Identifier

import System.Console.Pretty (color, Color(Green))
import System.IO (hPutStrLn, stderr)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, maybeToList)
import Data.UUID (UUID)
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

relationContainsIdentifier :: Identifiable a => a -> Relation -> Bool
relationContainsIdentifier a (Relation src _ target) = any (`matchesIdentifiable` a) [src, target]

{-|
  direction should always be from the smaller to the bigger in which it is included
-}

data Relations
  = Relations (Vector Relation)
  deriving (Eq, Show, Generic)
instance A.ToJSON Relations
instance A.FromJSON Relations
instance Semigroup Relations where
    (Relations rs1) <> (Relations rs2) = Relations (vNub (rs1 <> rs2))
instance Monoid Relations where
    mempty = Relations mempty