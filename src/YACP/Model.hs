{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Model
  ( module X
  , State (..), Components (..), Relations (..)
  , YACP (..), runYACP, runYACP'
  , addComponent, addComponents
  , addRelation, addRelations
  , addComponentsWithRelations
  -- misc
  , stderrLog
  ) where

import YACP.MyPrelude
import YACP.Model.Identifier as X
import YACP.Model.Component as X
import YACP.Model.Relation as X

import qualified Data.Vector as V
import qualified Control.Monad.State as MTL
import           System.Console.Pretty (color, Color(Green))
import           System.IO (hPutStrLn, stderr)

data Components
  = Components (Vector Component)
  deriving (Eq, Show)

data Relations
  = Relations (Vector Relation)
  deriving (Eq, Show)

data State
  = State
  { _getRoots :: [Identifier]
  , _getComponents :: Components
  , _getRelations :: Relations
  } deriving (Eq, Show)

type YACP a
  = MTL.StateT State IO a
runYACP :: YACP a -> IO (a, State)
runYACP yacp = let
  initialState = State [] (Components V.empty) (Relations V.empty)
  in runYACP' yacp initialState
runYACP' :: YACP a -> State -> IO (a, State)
runYACP' yacp initialState = MTL.runStateT yacp initialState
stderrLog :: String -> YACP ()
stderrLog msg = MTL.liftIO $ hPutStrLn stderr (color Green msg)

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
  in \r@(Relation src _ target) -> do
  r' <- addRelationEdgesToComponents r
  MTL.modify (\s@State{_getRelations = rs} -> s{_getRelations = r' `addRelation'` rs})
addRelations :: Vector Relation -> YACP ()
addRelations = V.mapM_ addRelation

addComponentsWithRelations :: Vector (Component, [Relation]) -> YACP ()
addComponentsWithRelations cWRs = do
  addComponents (V.map (\(c,_) -> c) cWRs)
  addRelations (V.concatMap (\(_,rs) -> V.fromList rs) cWRs)
