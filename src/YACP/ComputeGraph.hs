{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.ComputeGraph
  ( computeGraph, computeGraphWithDepths
  , ppGraph
  ) where

import YACP.Core

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.Graph.Inductive.PatriciaTree as UG
import qualified Control.Monad.State as MTL

computeNodes :: YACP [G.LNode Component]
computeNodes = MTL.gets _getComponents >>= \(Components cs) ->
  return $ ((zip [1..]) . V.toList) cs

iToNode :: Identifiable a => [G.LNode a] -> Identifier -> Maybe G.Node
iToNode nodes i = case List.find (\(_,c) -> i `matchesIdentifiable` c) nodes of
  Just (i,_) -> Just i
  Nothing    -> Nothing

computeEdges :: [G.LNode Component] -> YACP [G.LEdge Relation]
computeEdges nodes = let
  computeEdge :: Relation -> [G.LEdge Relation]
  computeEdge r@(Relation rSrc rType rTarget) = let
    in case (nodes `iToNode` rSrc, nodes `iToNode` rTarget) of
    (Just rSrcN, Just rTargetN) -> [(rSrcN, rTargetN, r)]
    _                           -> []
  in MTL.gets _getRelations >>= \(Relations rs) -> let
  edges = (List.nub . concatMap computeEdge . V.toList) rs
  in return $ edges

computeGraph :: YACP (UG.Gr Component Relation)
computeGraph = do
  nodes <- computeNodes
  edges <- computeEdges nodes
  return (G.mkGraph nodes edges)

computeGraphWithDepths :: YACP (UG.Gr (Component, Int) Relation)
computeGraphWithDepths = do
  roots <- MTL.gets _getRoots
  graph <- computeGraph
  let nodes = G.labNodes graph
  let edges = G.labEdges graph
  let flippedGraph = G.mkGraph nodes (map (\(n1, n2, r) -> (n2, n1, r)) edges) :: (UG.Gr Component Relation)
  let rootNodes = (map fst . filter (\(_,c) -> any (`matchesIdentifiable` c) roots)) nodes
  let depths = map (`G.level` flippedGraph) rootNodes
  let getDepth n = case (map snd . mapMaybe (List.find (\(n',_) -> n == n'))) depths of
                     [] -> -1
                     ds -> minimum ds
  let nodes' = map (\(n, c) -> (n, (c, getDepth n))) nodes

  return $ G.mkGraph nodes' edges

ppGraph :: YACP (UG.Gr Component Relation)
ppGraph = do
  graph <- computeGraph
  (MTL.liftIO . G.prettyPrint . G.emap (\(Relation _ r _) -> r) . G.nmap show) graph
  return graph
