{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Core.StateActions
  ( addRoot, addRoots
  , addComponent, addComponents
  , addRelation, addRelations
  , addComponentsWithRelations
  -- misc
  , stderrLog
  ) where

import YACP.Core.Model

import System.Console.Pretty (color, Color(Green))
import System.IO (hPutStrLn, stderr)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import System.Random (randomIO)
import qualified Control.Monad.State as MTL
import qualified Data.Aeson as A (Array)
import qualified Data.Monoid (mconcat)
import qualified Data.Vector as V
import qualified Distribution.SPDX as SPDX
import qualified Distribution.SPDX.Extra as SPDX
import qualified Network.URI as URI
import qualified System.FilePath as FP

stderrLog :: String -> YACP ()
stderrLog msg = MTL.liftIO $ hPutStrLn stderr (color Green msg)

addRoot :: Identifier -> YACP ()
addRoot r = MTL.modify (\s@State{_getRoots = rs} -> s{_getRoots = r:rs})
addRoots :: Vector Identifier -> YACP ()
addRoots = V.mapM_ addRoot

addComponent :: Component -> YACP Identifier
addComponent = let
  addComponent' :: Component -> Components -> Components
  addComponent' c (Components cs) = let
    identifier = getIdentifier c
    nonMatchingCs = V.filter (not . (identifier `matchesIdentifiable`)) cs
    matchingCs = V.filter (identifier `matchesIdentifiable`) cs
    mergedC = c <> ((mconcat . V.toList) matchingCs)
    in Components (mergedC `V.cons` nonMatchingCs)
  in \c -> do
  mapM_ addRelation (_getComponentRelations c)
  mapM_ addComponent (_getComponentSubComponents c)
  c' <- MTL.liftIO $ addUuidIfMissing c
  MTL.modify (\s@State{_getComponents = cs} -> s{_getComponents = c' `addComponent'` cs})
  c'' <- MTL.gets (\State{_getComponents = Components cs} ->
                     case (V.find ((getIdentifier c') `matchesIdentifiable`)  cs) of
                       Just c'' -> c''
                       Nothing  -> c')
  return (getIdentifier c'')
addComponents :: Vector Component -> YACP ()
addComponents = V.mapM_ addComponent

addRelation :: Relation -> YACP ()
addRelation = let
  addRelation' :: Relation -> Relations -> Relations
  addRelation' r (Relations rs) = Relations (r `V.cons` rs)
  addRelationEdgesToComponents :: Relation -> YACP Relation
  addRelationEdgesToComponents (r@(Relation src _ target)) = do
    src' <- addComponent (identifierToComponent src)
    target' <- addComponent (identifierToComponent target)
    return r{ _getRelationSrc = src'
            , _getRelationTarget = target'
            }

  flipDirection :: Relation -> Relation
  flipDirection (r@Relation{_getRelationSrc = src, _getRelationTarget = target}) = r{_getRelationSrc = target, _getRelationTarget = src}
  normalizeRelation :: Relation -> Relation
  normalizeRelation (r@Relation{_getRelationType = DEPENDS_ON})       = flipDirection (r{_getRelationType = DEPENDENCY_OF})
  normalizeRelation (r@Relation{_getRelationType = DESCRIBED_BY})     = flipDirection (r{_getRelationType = DESCRIBES})
  normalizeRelation (r@Relation{_getRelationType = CONTAINS})         = flipDirection (r{_getRelationType = CONTAINED_BY})
  normalizeRelation (r@Relation{_getRelationType = HAS_PREREQUISITE}) = flipDirection (r{_getRelationType = PREREQUISITE_FOR})
  normalizeRelation r                                                 = r

  in \r -> do
  r' <- addRelationEdgesToComponents (normalizeRelation r)
  MTL.modify (\s@State{_getRelations = rs} -> s{_getRelations = r' `addRelation'` rs})
addRelations :: Vector Relation -> YACP ()
addRelations = V.mapM_ addRelation

addComponentsWithRelations :: Vector (Component, [Relation]) -> YACP ()
addComponentsWithRelations cWRs = do
  addComponents (V.map (\(c,_) -> c) cWRs)
  addRelations (V.concatMap (\(_,rs) -> V.fromList rs) cWRs)
