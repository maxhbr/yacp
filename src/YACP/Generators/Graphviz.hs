{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Generators.Graphviz
  ( computeDiagram
  ) where

import YACP.Core

import qualified Diagrams.TwoD.GraphViz as G

-- computeDiagram :: StateContainer -> G.Gr Identifier e
computeDiagram = undefined
