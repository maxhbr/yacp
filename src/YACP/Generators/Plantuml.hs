{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Generators.Plantuml
  ( writePlantuml, writePlantuml'
  , writePlantumlFile
  ) where

import YACP.MyPrelude
import YACP.Model
import YACP.Processors.ComputeGraph (computeComponentsMapping)

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.Graph as G
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Control.Monad.State as MTL
import qualified System.Process as P


showVertex = ('C':) . show

writeComponents :: Handle -> (G.Vertex -> Maybe Component) -> G.Bounds -> IO ()
writeComponents h vertToC (lower,upper) = let
  assocs = map (\v -> (showVertex v, vertToC v)) [lower..upper]
  formatPair :: (String, Maybe Component) -> IO ()
  formatPair (key, Just c) = hPutStrLn h $ "component \"" ++ show (getIdentifier c) ++ "\" as " ++ key
  formatPair _             = return ()
  in mapM_ (formatPair) assocs

writeRelations :: Handle -> (Identifier -> Maybe G.Vertex) -> Vector Relation -> IO ()
writeRelations h iToVert = let
  writeRelation :: Relation -> IO ()
  writeRelation (Relation rSrc rType rTarget) = case (iToVert rSrc, iToVert rTarget) of
    (Just vSrc, Just vTarget) ->
      (hPutStrLn h) $ showVertex vSrc ++ " ---> " ++ showVertex vTarget ++ " <<" ++ show rType ++ ">>"
    _ -> return ()
  in V.mapM_ writeRelation


writePlantuml :: Handle -> YACP ()
writePlantuml h = MTL.get >>= \(State
                 { _getRoots = roots
                 , _getComponents = (Components cs)
                 , _getRelations = (Relations rs)
                 }) -> let
  writeHeader = do
    hPutStrLn h "@startuml"
    hPutStrLn h "left to right direction"
  writeFooter = do
    hPutStrLn h "@enduml"
  in do
  (vertToC, iToVert, bounds) <- computeComponentsMapping
  MTL.liftIO $ do
    writeHeader
    writeComponents h vertToC bounds
    writeRelations h iToVert rs
    writeFooter

writePlantuml' :: YACP ()
writePlantuml' = writePlantuml stdout

writePlantumlFile :: FilePath -> YACP ()
writePlantumlFile fp = do
  state <- MTL.get
  MTL.liftIO $ do
    IO.withFile fp IO.WriteMode $
      \h -> runYACP' (writePlantuml h) state
    P.callProcess "plantuml" ["-v", "-progress", "-tsvg", fp]
  return ()
