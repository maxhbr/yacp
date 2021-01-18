{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Processors.PPState
  ( ppState
  ) where

import YACP.MyPrelude
import YACP.Model
import YACP.Processors.ComputeGraph

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Graph as G
import qualified Control.Monad.State as MTL

ppComponents :: (G.Vertex -> Maybe Component) -> G.Bounds -> YACP ()
ppComponents vertToC (lower,upper) = let
  assocs = map (\v -> (v, vertToC v)) [lower..upper]
  formatPair :: (G.Vertex, Maybe Component) -> IO ()
  formatPair (key, Just c) = putStrLn $
    show key ++ ":\t" ++ show (getIdentifier c)
  formatPair _             = return ()
  in MTL.liftIO $ mapM_ formatPair assocs

ppRelations :: (G.Edge -> [Relation]) -> G.Graph -> YACP ()
ppRelations edgeToRs graph = let
  edges = G.edges graph
  ppEdge :: G.Edge -> YACP ()
  ppEdge edge = let
    relations = edgeToRs edge
    in MTL.liftIO $ do
    putStrLn (show edge ++ ":\t" ++ show (map _getRelationType relations))
  in mapM_ ppEdge edges

ppState :: YACP ()
ppState = MTL.get >>= \(State
               { _getRoots = roots
               , _getComponents = (Components cs)
               , _getRelations = (Relations rs)
               }) -> let
  in do
  (graph, vertToC, iToVert, bounds, edgeToRs) <- computeGraph
  ppComponents vertToC bounds
  ppRelations edgeToRs graph
