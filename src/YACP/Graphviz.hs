{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Graphviz
  ( computeDigraph
  ) where

import YACP.Core
import YACP.ComputeGraph

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Graph as G
import qualified Data.Graph.Inductive.Graph as G
import qualified Control.Monad.State as MTL
import qualified Data.GraphViz as GV
-- import qualified Data.GraphViz.Types as GV
-- import qualified Data.GraphViz.Types.Graph as GV
-- import qualified Data.GraphViz.Types.Monadic as GV
-- import qualified Data.GraphViz.Types.Generalised as GV

computeDigraph :: YACP (GV.DotGraph G.Node)
computeDigraph = do
  (graph, vertToC, iToVert, bounds, edgeToRs) <- computeGraph
  let graphvizParams = undefined
  return (GV.graphToDot graphvizParams graph)
