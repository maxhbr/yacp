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

import YACP.Core
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

writeComponents :: Handle -> (G.Vertex -> Maybe Component) -> G.Bounds -> [Identifier] -> IO ()
writeComponents h vertToC (lower,upper) roots = let
  assocs = map (\v -> (showVertex v, vertToC v)) [lower..upper]
  tagFromComponent :: Component -> String
  tagFromComponent c = let
    isRoot :: Component -> Bool
    isRoot c' = any (`matchesIdentifiable` c') roots
    in if isRoot c
       then "<<ROOT>>"
       else ""
  formatPair :: (String, Maybe Component) -> IO ()
  formatPair (key, Just c) = hPutStrLn h $ unwords ["component"
                                                   , "\"" ++ show (getIdentifier c) ++ "\\n" ++ showLicense c ++ "\""
                                                   , tagFromComponent c
                                                   , "as", key]
  formatPair _             = return ()
  in mapM_ formatPair assocs

writeRelations :: Handle -> (Identifier -> Maybe G.Vertex) -> Vector Relation -> IO ()
writeRelations h iToVert = let
  writeRelation :: Relation -> IO ()
  writeRelation (Relation rSrc rType rTarget) = case (iToVert rSrc, iToVert rTarget) of
    (Just vSrc, Just vTarget) -> let
      arrowArgs = case rType of
        GENERATES -> "-"
        GENERATED_FROM -> "-"
        METAFILE_OF -> "-"
        CONTAINED_BY -> ""
        _ -> ""
      rtlOrLtr = let
        rtl = [ showVertex vTarget , "<-" ++ arrowArgs ++ "-" , showVertex vSrc ]
        ltr = [ showVertex vSrc , "-" ++ arrowArgs ++ "->" , showVertex vTarget ]
        in case rType of
             GENERATES -> ltr
             GENERATED_FROM -> ltr
             METAFILE_OF -> ltr
             CONTAINED_BY -> ltr
             _ -> rtl
      in
      hPutStrLn h $ unwords (rtlOrLtr ++
                             [ "<<" ++ show rType ++ ">>"
                             , ( case rType of
                                   DEPENDENCY_OF -> ""
                                   _             -> ": " ++ show rType) ])
    _ -> return ()
  in V.mapM_ writeRelation


writePlantuml :: Handle -> YACP ()
writePlantuml h = MTL.get >>= \(State
                 { _getRoots = roots
                 , _getComponents = (Components cs)
                 , _getRelations = (Relations rs)
                 }) -> let
  -- writeArrowHeader name style =
  writeHeader = do
    hPutStrLn h "@startuml"
    hPutStrLn h "left to right direction"
    hPutStrLn h (
      unlines
        [ "skinparam component {"
        , "    StereotypeFontSize 20"
        , "    shadowing false"
        , ""
        , "    StereotypeFontColor #black"
        , "    FontColor #black"
        , "    BackgroundColor #c0e8b5"
        , "    BorderColor #073B6F"
        , "}"
        , "skinparam component<<ROOT>> {"
        , "    StereotypeFontSize 0"
        , "    BorderColor #red"
        , "    BackgroundColor #coral"
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
  (vertToC, iToVert, bounds) <- computeComponentsMapping
  MTL.liftIO $ do
    writeHeader
    writeComponents h vertToC bounds roots
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
