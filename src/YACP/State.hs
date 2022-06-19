{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.State
  ( module X
  ) where

import YACP.Model as X

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

data YACPIssue
  = YACPParsingIssue String
  | YACPFileParsingIssue FilePath String
  deriving (Eq, Show, Generic)
instance A.ToJSON YACPIssue
instance A.FromJSON YACPIssue

data State
  = State
  { _getRoots :: [Identifier]
  , _getComponents :: Components
  , _getRelations :: Relations
  , _getFiles :: Files  
  , _getYacpIssues :: [YACPIssue]
  } deriving (Eq, Show, Generic)
instance A.ToJSON State
instance A.FromJSON State

type YACP a
  = MTL.StateT State IO a
runYACP :: YACP a -> IO (a, State)
runYACP yacp = let
  initialState = State mempty mempty mempty mempty mempty 
  in runYACP' yacp initialState
runYACP' :: YACP a -> State -> IO (a, State)
runYACP' yacp initialState = MTL.runStateT yacp initialState