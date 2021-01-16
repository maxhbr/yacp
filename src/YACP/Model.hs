{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Model
  ( module X
  , YACP (..)
  , addComponent, addComponents
  , addRelation, addRelations
  ) where


import YACP.MyPrelude
import YACP.Model.Identifier as X
import YACP.Model.Component as X
import YACP.Model.Relation as X

import qualified Data.Vector as V
import qualified Control.Monad.State as MTL

data Components
  = Components (Vector Component)

data Relations
  = Relations (Vector Relation)

data StateContainer
  = StateContainer
  { _getComponents :: Components
  , _getRelations :: Relations
  }

type YACP
  = MTL.StateT StateContainer IO ()

addComponent :: Component -> YACP
addComponent = let
  addComponent' :: Component -> Components -> Components
  addComponent' c (Components cs) = let
    identifier = getIdentifier c
    nonMatchingCs = V.filter (not . (identifier `matchesIdentifiable`)) cs
    matchingCs = V.filter (identifier `matchesIdentifiable`) cs
    mergedC = c <> ((mconcat . V.toList) matchingCs)
    in Components (mergedC `V.cons` nonMatchingCs)
  in \c -> do
  c' <- MTL.liftIO $ addUuidIfMissing c
  MTL.modify (\s@StateContainer{_getComponents = cs} -> s{_getComponents = c' `addComponent'` cs})
addComponents :: Vector Component -> YACP
addComponents = V.mapM_ addComponent

addRelation :: Relation -> YACP
addRelation = let
  addRelation' :: Relation -> Relations -> Relations
  addRelation' r (Relations rs) = Relations (r `V.cons` rs)
  addRelationEdgesToComponents :: Relation -> YACP
  addRelationEdgesToComponents (Relation src _ target) = do
    addComponent (identifierToComponent src)
    addComponent (identifierToComponent target)
  in \r -> do
  MTL.modify (\s@StateContainer{_getRelations = rs} -> s{_getRelations = r `addRelation'` rs})
  addRelationEdgesToComponents r
addRelations :: Vector Relation -> YACP
addRelations = V.mapM_ addRelation
