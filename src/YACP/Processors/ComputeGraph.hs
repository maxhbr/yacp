{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Processors.ComputeGraph
  ( computeGraph
  ) where

import YACP.MyPrelude
import YACP.Model

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Graph as G -- http://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Graph.html#t:Graph

computeComponentsMap :: Vector Component -> [(String, Component)]
computeComponentsMap = (zip (map (('P':) . show) [0..])) . V.toList
