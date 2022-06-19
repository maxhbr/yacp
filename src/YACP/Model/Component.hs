{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Model.Component
  ( Component (..)
  , identifierToComponent
  , Components (..)
  ) where

import YACP.MyPrelude
import YACP.Model.Identifier
import YACP.Model.License
import YACP.Model.Relation

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

data Component
  = Component
  { _getComponentIdentifier :: Identifier
  , _getComponentLicense :: MaybeLicenseExpression
  , _getComponentPayload :: A.Array
  , _getComponentRelations :: Relations
  , _getComponentSubComponents :: Components
  } deriving (Eq, Generic)
instance A.ToJSON Component
instance A.FromJSON Component
instance Show Component where
  show (Component{_getComponentIdentifier = cId, _getComponentLicense = l}) = "{{{" ++  show cId ++ "@" ++ show l ++ "}}}"
instance Identifiable Component where
  getIdentifier = _getComponentIdentifier
  addIdentifier (c@Component{_getComponentIdentifier = is}) i = c{_getComponentIdentifier = is<>i}
instance Semigroup Component where
  c1 <> c2 = let
    mergedIdentifiers = (getIdentifier c1) <> (getIdentifier c2)
    mergedLicense = let
       l1 = _getComponentLicense c1
       l2 = _getComponentLicense c2
      in l1 <> l2
    mergedPayload = let
      p1 = _getComponentPayload c1
      p2 = _getComponentPayload c2
      in if p1 /= p2
         then p1 <> p2
         else p1
    mergedRelations = let
      r1 = _getComponentRelations c1
      r2 = _getComponentRelations c2
      in r1 <> r2
    mergedSubComponents = let
      sc1 = _getComponentSubComponents c1
      sc2 = _getComponentSubComponents c2
      in sc1 <> sc2
    in Component
       { _getComponentIdentifier = mergedIdentifiers
       , _getComponentLicense = mergedLicense
       , _getComponentPayload = mergedPayload
       , _getComponentRelations = mergedRelations
       , _getComponentSubComponents = mergedSubComponents
       }
instance Monoid Component where
  mempty = Component mempty mempty mempty mempty mempty

identifierToComponent :: Identifier -> Component
identifierToComponent i = mempty{_getComponentIdentifier = i}

instance Licenseable Component where
  getLicense = _getComponentLicense

data Components
  = Components (Vector Component)
  deriving (Eq, Show, Generic)
instance A.ToJSON Components
instance A.FromJSON Components
instance Semigroup Components where
    (Components cs1) <> (Components cs2) = Components (vNub (cs1 <> cs2))
instance Monoid Components where
    mempty = Components mempty