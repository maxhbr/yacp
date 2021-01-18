{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module YACP.Model.File
  ( File (..)
  ) where

import YACP.MyPrelude
import YACP.Model.Identifier
import YACP.Model.Component

import qualified Data.Aeson as A (Array)
import qualified Data.Monoid (mconcat)
import qualified Distribution.SPDX as SPDX
import qualified Distribution.SPDX.Extra as SPDX

data File
  = File
  { _getFilePath :: FilePath
  , _getFileOtherIdentifier :: Identifier
  } deriving (Eq, Show)

instance Identifiable File where
  getIdentifier f = (PathIdentifier $ _getFilePath f) <> _getFileOtherIdentifier f
  addIdentifier (f@File{_getFileOtherIdentifier = is}) i = f{_getFileOtherIdentifier = is<>i}
