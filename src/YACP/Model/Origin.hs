{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Model.Origin
  ( Origin (..)
  ) where

import YACP.MyPrelude
import YACP.Model.Identifier

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

data Origin = Origin
  { _getOriginIdentifier :: Identifier
  } deriving (Eq, Generic)
instance A.ToJSON Origin
instance A.FromJSON Origin
instance Show Origin where
  show (Origin{_getOriginIdentifier = oId}) = "{{{" ++  show oId ++ "}}}"
instance Identifiable Origin where
  getIdentifier = _getOriginIdentifier
  addIdentifier (c@Origin{_getOriginIdentifier = is}) i = c{_getOriginIdentifier = is<>i}
