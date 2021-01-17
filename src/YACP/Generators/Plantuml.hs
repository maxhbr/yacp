{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Generators.Plantuml
  ( writePlantuml, writePlantuml'
  ) where

import YACP.MyPrelude
import YACP.Model

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V

computeComponentsMap :: Vector Component -> [(String, Component)]
computeComponentsMap = (zip (map (('P':) . show) [0..])) . V.toList

writeComponents :: Handle -> [(String, Component)] -> IO ()
writeComponents h assocs = let
  formatPair :: (String, Component) -> String
  formatPair (key, c) = "[" ++ show (getIdentifier c) ++ "] as " ++ key
  in mapM_ ((hPutStrLn h) . formatPair) assocs

writeRelations :: Handle -> [(String, Component)] -> Vector Relation -> IO ()
writeRelations h assocs = let
  lookupId :: Identifier -> String
  lookupId i = case List.find (\(_,c) -> i `matchesIdentifiable` c) assocs of
    Just (k,_) -> k
    Nothing -> "BOTTOM"
  writeRelation :: Relation -> IO ()
  writeRelation (Relation rSrc rType rTarget) = let
    kSrc = lookupId rSrc
    kTarget = lookupId rTarget
    in (hPutStrLn h) $ kSrc ++ " ---> " ++ kTarget ++ " <<" ++ show rType ++ ">>"

  in V.mapM_ writeRelation


writePlantuml :: Handle -> State -> IO ()
writePlantuml h (State
                 { _getRoots = roots
                 , _getComponents = (Components cs)
                 , _getRelations = (Relations rs)
                 }) = let
  assocs = computeComponentsMap cs
  writeHeader = do
    hPutStrLn h "@startuml"
    hPutStrLn h "left to right direction"
  writeFooter = do
    hPutStrLn h "@enduml"
  in do
  writeHeader
  writeComponents h assocs
  writeRelations h assocs rs
  writeFooter

writePlantuml' :: State -> IO ()
writePlantuml' = writePlantuml stdout
