{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP
  ( module X
  , argsToYACP, argsToYACP'
  , parseScancodeFile
  , parseSPDXFile
  , parseCycloneDXFile
  , parseHHCFile
  ) where

import YACP.Core as X
import YACP.OrtCollector as X
import YACP.ScancodeCollector as X
import YACP.SPDXCollector as X
import YACP.CycloneDXCollector as X
import YACP.ComputeGraph as X
import YACP.PPState as X
import YACP.StateWriter as X
import YACP.Plantuml as X
import YACP.Graphviz as X
import YACP.HHCWriter as X
import YACP.HHCCollector as X

import System.Environment (getArgs)
import System.IO
import System.Exit
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.State as MTL

parseBSFromFile :: (B.ByteString -> YACP a) -> FilePath -> YACP a
parseBSFromFile fun path = do
  bs <- MTL.liftIO $ B.readFile path
  fun bs

parseScancodeFile :: FilePath -> YACP ()
parseScancodeFile = parseBSFromFile parseScancodeBS

parseSPDXFile :: FilePath -> YACP ()
parseSPDXFile = parseBSFromFile parseSPDXBS

parseCycloneDXFile :: FilePath -> YACP ()
parseCycloneDXFile = parseBSFromFile parseCycloneDXBS

parseHHCFile :: FilePath -> YACP ()
parseHHCFile = parseBSFromFile parseHHCBS


argsToYACP' :: [String] -> YACP ()
argsToYACP' [] = return ()
argsToYACP' [outDir] = do
  ppState
  MTL.liftIO $ createDirectoryIfMissing True outDir
  writeStateFile (outDir </> "_state.json")
  writePlantumlFile (outDir </> "plantuml.puml")
  writeDigraphFile (outDir </> "digraph.dot")
  writeHHCFile (outDir </> "hhc.json")
argsToYACP' ("--sc": (f: oArgs)) = (parseScancodeFile f) >> (argsToYACP' oArgs)
argsToYACP' ("--ort": (f: oArgs)) = (parseOrtFile f) >> (argsToYACP' oArgs)
argsToYACP' ("--spdx": (f: oArgs)) = (parseSPDXFile f) >> (argsToYACP' oArgs)
argsToYACP' (unknown: oArgs) = MTL.liftIO $ do
  putStrLn ("failed to parse: " ++ unknown)
  exitFailure


argsToYACP :: IO ()
argsToYACP = let
    help :: IO()
    help = putStrLn $ unlines
      [ "yacp - yet another compliance platform"
      , "$0 $inputs $outputFolder"
      , "  where $inputs are multiples of"
      , "    --sc $scancode_file <- parse scancode file"
      , "    --ort $ort_file <- parse ort file"
      , "    --spdx $spdx_file <- parse spdx json file"
      , "  and $outputFolder <- dir to write files to"
      , "$0 [-h|--help] <- show this msg"
      ]
  in getArgs >>= \case
    [] -> help
    ["-h"] -> help
    ["--help"] -> help
    args -> do
      (_, state) <- runYACP (argsToYACP' args)
      return ()