{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module YACP.Model.Component
  ( Component (..)
  , identifierToComponent
  ) where

import YACP.MyPrelude
import YACP.Model.Identifier

import qualified Data.Aeson as A (Array)
import qualified Data.Monoid (mconcat)
import qualified Distribution.SPDX as SPDX
import qualified Distribution.SPDX.Extra as SPDX

-- data ComponentType
--   = File
--   | Package
--   deriving (Eq, Show)

{-|
  Class for Component
-}
data Component
  = Component
  { _getIdentifier :: Identifier
  , _getLicense :: Maybe SPDX.LicenseExpression
  , _getPayload :: A.Array
  }
instance Identifiable Component where
  getIdentifier = _getIdentifier
  addIdentifier (c@Component{_getIdentifier = is}) i = c{_getIdentifier = is<>i}
instance Semigroup Component where
  c1 <> c2 = let
    mergedIdentifiers = (getIdentifier c1) <> (getIdentifier c2)
    mergedLicense = let
       l1 = _getLicense c1
       l2 = _getLicense c2
      in case l1 of
        Nothing  -> l2
        Just l1' -> case l2 of
          Nothing  -> l1
          Just l2' -> Just (l1' `SPDX.EOr` l2')
    mergedPayload = let
      p1 = _getPayload c1
      p2 = _getPayload c2
      in if p1 /= p2
         then p1 <> p2
         else p1
    in Component
       { _getIdentifier = mergedIdentifiers
       , _getLicense = mergedLicense
       , _getPayload = mergedPayload
       }
instance Monoid Component where
  mempty = Component mempty Nothing mempty

identifierToComponent :: Identifier -> Component
identifierToComponent i = mempty{_getIdentifier = i}
