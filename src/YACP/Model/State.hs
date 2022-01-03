{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeFamilies              #-}

module YACP.Model.State
  ( State(..)
  , Components(..)
  , Relations(..)
  , Files(..)
  , YACPIssue(..)
  , YACP(..)
  , runYACP
  , runYACP'
  ) where

import           YACP.Model.Component            as X
import           YACP.Model.File                 as X
import           YACP.Model.Identifier           as X
import           YACP.Model.Relation             as X
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

--------------------------------------------------------------------------------
data Components =
  Components (Vector Component)
  deriving (Eq, Show, Generic)

instance A.ToJSON Components

instance A.FromJSON Components

data Relations =
  Relations (Vector Relation)
  deriving (Eq, Show, Generic)

instance A.ToJSON Relations

instance A.FromJSON Relations

data Files =
  Files (Vector File)
  deriving (Eq, Show, Generic)

instance A.ToJSON Files

instance A.FromJSON Files

data YACPIssue
  = YACPParsingIssue String
  | YACPFileParsingIssue FilePath String
  deriving (Eq, Show, Generic)

instance A.ToJSON YACPIssue

instance A.FromJSON YACPIssue

data State =
  State
    { _getRoots      :: [Identifier]
    , _getComponents :: Components
    , _getRelations  :: Relations
    , _getFiles      :: Files
    , _getYacpIssues :: [YACPIssue]
    }
  deriving (Eq, Show, Generic)

instance A.ToJSON State

instance A.FromJSON State

type YACP a = MTL.StateT State IO a

runYACP :: YACP a -> IO (a, State)
runYACP yacp =
  let initialState =
        State [] (Components V.empty) (Relations V.empty) (Files V.empty) []
   in runYACP' yacp initialState

runYACP' :: YACP a -> State -> IO (a, State)
runYACP' yacp initialState = MTL.runStateT yacp initialState
