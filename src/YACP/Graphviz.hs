{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Graphviz
  ( computeDigraph
  , writeDigraphFile
  , renderDigraphFile
  ) where

import YACP.Core
import YACP.ComputeGraph

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Graph.Inductive.Graph as G
import qualified Control.Monad.State as MTL
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Commands.IO as GV

computeDigraph :: YACP (GV.DotGraph G.Node)
computeDigraph = do
  graph <- computeGraph
  let graphvizParams = GV.nonClusteredParams
  let digraph = (GV.graphToDot graphvizParams graph)
  -- TODO: label and relation styling
  return digraph

writeDigraphFile :: FilePath -> YACP FilePath
writeDigraphFile fp = let
  format = GV.Svg
  in do
  stderrLog $ "writeDigraphFile " ++ fp
  digraph <- computeDigraph
  MTL.liftIO $ do
    GV.writeDotFile fp digraph
    return fp

renderDigraphFile :: FilePath -> YACP FilePath
renderDigraphFile fp = let
  format = GV.Svg
  in do
  stderrLog $ "writeDigraphFile " ++ fp
  digraph <- computeDigraph
  MTL.liftIO $ do
    GV.runGraphviz digraph format (fp ++ "." ++ (show format))
