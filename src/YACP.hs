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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.State as MTL

parseBSFromFile :: (B.ByteString -> YACP (Maybe YACPIssue)) -> FilePath -> YACP ()
parseBSFromFile fun path = do
  fileExist <- MTL.liftIO $ doesFileExist path
  unless fileExist $
    fail ("The file " ++ path ++ " was not found")
  bs <- MTL.liftIO $ B.readFile path
  maybeIssue <- fun bs
  case maybeIssue of 
    Nothing                     -> pure ()
    Just (YACPParsingIssue err) -> addYACPIssue (YACPFileParsingIssue path err)
    Just issue                  -> addYACPIssue issue

parseOrtFile :: FilePath -> YACP ()
parseOrtFile  = parseBSFromFile parseOrtBS

parseScancodeFile :: FilePath -> YACP ()
parseScancodeFile = parseBSFromFile parseScancodeBS

parseSPDXFile :: FilePath -> YACP ()
parseSPDXFile = parseBSFromFile parseSPDXBS

parseCycloneDXFile :: FilePath -> YACP ()
parseCycloneDXFile = parseBSFromFile parseCycloneDXBS

parseHHCFile :: FilePath -> YACP ()
parseHHCFile = parseBSFromFile parseHHCBS

failOnIssue :: YACP ()
failOnIssue = do
  issues <- MTL.gets (_getYacpIssues)
  case issues of
    [] -> pure ()
    is -> do 
      stderrLog ("Failed with " ++ (show (length is)) ++ " issues")
      MTL.liftIO exitFailure


argsToYACP' :: [String] -> YACP ()
argsToYACP' [] = return ()
argsToYACP' [outDir] = do
  -- ppState
  MTL.liftIO $ createDirectoryIfMissing True outDir
  writeStateFile (outDir </> "_state.json")
  writePlantumlFile (outDir </> "plantuml.puml")
  _ <- writeDigraphFile (outDir </> "digraph.dot")
  writeHHCFile (outDir </> "hhc.json")
  failOnIssue
argsToYACP' ("--sc": (f: oArgs)) = parseScancodeFile f >> argsToYACP' oArgs
argsToYACP' ("--ort": (f: oArgs)) = parseOrtFile f >> argsToYACP' oArgs
argsToYACP' ("--spdx": (f: oArgs)) = parseSPDXFile f >> argsToYACP' oArgs
argsToYACP' ("--hhc": (f: oArgs)) = parseHHCFile f >> argsToYACP' oArgs
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
