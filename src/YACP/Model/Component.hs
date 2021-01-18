{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module YACP.Model.Component
  ( Component (..)
  , identifierToComponent
  , Licenseable (..)
  ) where

import YACP.MyPrelude
import YACP.Model.Identifier

import qualified Data.Aeson as A (Array)
import qualified Data.Monoid (mconcat)
import qualified Distribution.SPDX as SPDX
import qualified Distribution.SPDX.Extra as SPDX

{-|
  Class for Component
-}
data Component
  = Component
  { _getComponentIdentifier :: Identifier
  , _getComponentLicense :: Maybe SPDX.LicenseExpression
  , _getComponentPayload :: A.Array
  } deriving (Eq)
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
      in case l1 of
        Nothing  -> l2
        Just l1' -> case l2 of
          Nothing  -> l1
          Just l2' -> Just (l1' `SPDX.EOr` l2')
    mergedPayload = let
      p1 = _getComponentPayload c1
      p2 = _getComponentPayload c2
      in if p1 /= p2
         then p1 <> p2
         else p1
    in Component
       { _getComponentIdentifier = mergedIdentifiers
       , _getComponentLicense = mergedLicense
       , _getComponentPayload = mergedPayload
       }
instance Monoid Component where
  mempty = Component mempty Nothing mempty

identifierToComponent :: Identifier -> Component
identifierToComponent i = mempty{_getComponentIdentifier = i}

class Licenseable a where
  getLicense :: a -> Maybe SPDX.LicenseExpression
  showLicense :: a -> String
  showLicense a = let
    showLicense' :: SPDX.LicenseExpression -> String
    showLicense' (SPDX.ELicense l _) = let
      showLicense'' :: SPDX.SimpleLicenseExpression -> String
      showLicense'' (SPDX.ELicenseId l') = show l'
      showLicense'' (SPDX.ELicenseRef l') = SPDX.licenseRef l'
      in showLicense'' l
    showLicense' (SPDX.EAnd l r) = unwords ["(", showLicense' l, "AND", showLicense' r, ")"]
    showLicense' (SPDX.EOr l r) = unwords ["(", showLicense' l, "OR", showLicense' r, ")"]
    in case getLicense a of
      Just l -> showLicense' l
      Nothing -> ""

instance Licenseable Component where
  getLicense = _getComponentLicense
