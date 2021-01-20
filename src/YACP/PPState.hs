{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.PPState
  ( ppState
  , ppFiles
  ) where

import YACP.Core
import YACP.ComputeGraph

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Graph.Inductive.Graph as G
import qualified Control.Monad.State as MTL

ppComponents :: [G.LNode Component] -> YACP ()
ppComponents nodes = let
  formatPair :: G.LNode Component -> IO ()
  formatPair (node, c) = putStrLn $
    unlines ((show node ++ ":"): ( map ( ('\t':) . show) . flattenIdentifierToList . getIdentifier) c)
  in MTL.liftIO (mapM_ formatPair nodes)

ppRelations :: [G.LEdge Relation] -> YACP ()
ppRelations = let
  ppEdge :: G.LEdge Relation -> YACP ()
  ppEdge (nSrc, nTarget, relation) = MTL.liftIO $ do
    putStrLn (show (nSrc,nTarget) ++ ": " ++ show (_getRelationType relation))
  in mapM_ ppEdge

ppFiles :: YACP ()
ppFiles = let
  ppFile (f@(File path ids lic)) = putStrLn $
    path
    ++ (case showLicense f of
           "" -> ""
           l -> "\n\tlicense: " ++ l)
    ++ (if ids == mempty
         then ""
         else "\n\tids: " ++ show ids)
  in do
  Files fs <- MTL.gets _getFiles
  MTL.liftIO $ V.mapM_ ppFile fs


ppState :: YACP ()
ppState = do
  graph <- computeGraph
  ppComponents (G.labNodes graph)
  ppRelations (G.labEdges graph)
