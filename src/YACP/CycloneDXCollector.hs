{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.CycloneDXCollector
  ( CycloneDXFile (..)
  , parseCycloneDXBS
  ) where

import YACP.Core
import YACP.ParserHelper

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List (intercalate)
import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.FilePath as FP
import qualified Control.Monad.State as MTL
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec as SPDX

parseCycloneDXBS = undefined
