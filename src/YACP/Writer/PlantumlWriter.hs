{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Plantuml
  ( writePlantuml, writePlantuml'
  , writePlantumlFile
  ) where

import YACP.Core
import YACP.ComputeGraph

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Control.Monad.State as MTL
import qualified System.Process as P
import qualified Data.Graph.Inductive.Graph as G

showVertex = ('C':) . show

writeComponents :: Handle -> [G.LNode (Component, Int)] -> [Identifier] -> IO ()
writeComponents h nodes roots = let
  tagFromComponent :: Component -> Int -> String
  tagFromComponent c depth = let
    isRoot :: Component -> Bool
    isRoot c' = any (`matchesIdentifiable` c') roots
    in if isRoot c
       then "<<ROOT>>"
       else case depth of
              -1 -> ""
              _  -> "<<DEPTH" ++ show depth ++ ">>"
  writeComponent :: (G.Node, (Component, Int)) -> IO ()
  writeComponent (key, (c, depth)) = hPutStrLn h $
    unwords [ "component"
            , "\"" ++ ( concatMap ( (++ "\\n") . show) . flattenIdentifierToList . getIdentifier) c ++ showLicense c ++ "\""
            , tagFromComponent c depth
            , "as", showVertex key
            ]
  in mapM_ writeComponent nodes

writeRelations :: Handle -> [G.LEdge Relation] -> IO ()
writeRelations h = let
  writeRelation :: G.LEdge Relation -> IO ()
  writeRelation (nSrc, nTarget, (Relation _ rType _)) = let
      arrowArgs = case rType of
        GENERATES -> ""
        GENERATED_FROM -> ""
        METAFILE_OF -> ""
        CONTAINED_BY -> ""
        _ -> ""
      rtlOrLtr = let
        rtl = [ showVertex nTarget , "<-" ++ arrowArgs ++ "-" , showVertex nSrc ]
        ltr = [ showVertex nSrc , "-" ++ arrowArgs ++ "->" , showVertex nTarget ]
        in case rType of
             GENERATES -> ltr
             GENERATED_FROM -> ltr
             METAFILE_OF -> ltr
             CONTAINED_BY -> ltr
             _ -> rtl
      in hPutStrLn h $ unwords ( rtlOrLtr ++
                                 [ "<<" ++ show rType ++ ">>"
                                 , ( case rType of
                                       DEPENDENCY_OF -> ""
                                       _             -> ": " ++ show rType ) ] )
  in mapM_ writeRelation


writePlantuml :: Handle -> YACP ()
writePlantuml h = MTL.get >>= \(State
                 { _getRoots = roots
                 , _getComponents = (Components cs)
                 , _getRelations = (Relations rs)
                 }) -> let
  writeHeader = do
    hPutStrLn h "@startuml"
    hPutStrLn h "left to right direction"
    hPutStrLn h (
      unlines
        [ "skinparam component {"
        , "    StereotypeFontSize 20"
        , "    shadowing false"
        , "    StereotypeFontColor #black"
        , "    FontColor #black"
        , "    BackgroundColor #ffffff"
        , "    BorderColor #073B6F"
        , "}"
        , "skinparam component<<ROOT>> {"
        , "    StereotypeFontSize 0"
        , "    BorderColor #red"
        , "    BackgroundColor #coral"
        , "}"
        , "skinparam component<<DEPTH1>> {"
        , "    BackgroundColor #a9cc9f"
        , "}"
        , "skinparam component<<DEPTH2>> {"
        , "    BackgroundColor #94b38b"
        , "}"
        , "skinparam component<<DEPTH3>> {"
        , "    BackgroundColor #7f9977"
        , "}"
        , "skinparam component<<DEPTH4>> {"
        , "    BackgroundColor #6a8063"
        , "}"
        , "skinparam component<<DEPTH5>> {"
        , "    BackgroundColor #546650"
        , "}"
        , "skinparam Arrow {"
        , "    Color #green"
        , "    FontColor #666666"
        , "    FontSize 16"
        , "    Thickness 2"
        , "}"
        , "skinparam Arrow<<GENERATES>> {"
        , "    Color #gray"
        , "    Thickness 1"
        , "}"
        , "skinparam Arrow<<GENERATED_FROM>> {"
        , "    Color #gray"
        , "    Thickness 1"
        , "}"
        , "skinparam Arrow<<METAFILE_OF>> {"
        , "    Color #gray"
        , "    Thickness 1"
        , "}"
        , "skinparam Arrow<<CONTAINED_BY>> {"
        , "    Color #gray"
        , "    Thickness 1"
        , "}"
        ])
  writeFooter = do
    hPutStrLn h "@enduml"
  in do
  graph <- computeGraphWithDepths
  MTL.liftIO $ do
    writeHeader
    writeComponents h (G.labNodes graph) roots
    writeRelations h (G.labEdges graph)
    writeFooter

writePlantuml' :: YACP ()
writePlantuml' = writePlantuml stdout

writePlantumlFile :: FilePath -> YACP ()
writePlantumlFile fp = do
  stderrLog $ "writePlantumlFile " ++ fp
  state <- MTL.get
  MTL.liftIO $ do
    IO.withFile fp IO.WriteMode $
      \h -> runYACP' (writePlantuml h) state
    P.callProcess "plantuml" ["-v", "-progress", "-tsvg", fp]
  return ()
