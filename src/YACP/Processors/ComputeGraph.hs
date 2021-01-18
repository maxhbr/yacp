{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Processors.ComputeGraph
  ( computeGraph
  , computeComponentsMapping
  ) where

import YACP.MyPrelude
import YACP.Model

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Graph as G -- http://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Graph.html#t:Graph
import qualified Control.Monad.State as MTL

computeComponentsMapping :: YACP (G.Vertex -> Maybe Component, Identifier -> Maybe G.Vertex, G.Bounds)
computeComponentsMapping = MTL.gets _getComponents >>= \(Components cs) -> let
  assocs :: [(G.Vertex, Component)]
  assocs = ((zip [1..])) . V.toList $ cs
  vertToC :: G.Vertex -> Maybe Component
  vertToC v = fmap (\(_,c) -> c) $ List.find (\(i,_) -> i == v) assocs
  iToVert :: Identifier -> Maybe G.Vertex
  iToVert i = fmap (\(v,_) -> v) $ List.find (\(_,c) -> i `matchesIdentifiable` c) assocs
  in return (vertToC, iToVert, (1, length assocs))

computeEdges :: (Identifier -> Maybe G.Vertex) -> YACP ([G.Edge], G.Edge -> [Relation])
computeEdges iToVert = let
  computeEdge :: Relation -> [(G.Edge, Relation)]
  computeEdge r@(Relation rSrc rType rTarget) = let
    in case (iToVert rSrc, iToVert rTarget) of
    (Just rSrcV, Just rTargetV) -> [((rSrcV, rTargetV), r)]
    _                           -> []
  in MTL.gets _getRelations >>= \(Relations rs) -> let
  result = (concatMap computeEdge . V.toList) rs
  edges = List.nub (map (\(e,_) -> e) result)
  edggeToRs e = (map (\(_,r) -> r) . filter (\(e',_) -> e == e')) result
  in return $ (edges, edggeToRs)

computeGraph :: YACP (G.Graph, G.Vertex -> Maybe Component, Identifier -> Maybe G.Vertex, G.Bounds, G.Edge -> [Relation])
computeGraph = do
  (vertToC, iToVert, bounds) <- computeComponentsMapping
  (edges, edgeToRs) <- computeEdges iToVert
  let graph = G.buildG bounds edges
  return (graph, vertToC, iToVert, bounds, edgeToRs)
