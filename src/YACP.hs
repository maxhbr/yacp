{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP
  ( module X
  , argsToYACP
  ) where

import           YACP.Core                     as X
import           YACP.Reader.ComponentDetectionReader
                                               as X
import           YACP.Reader.FosslightDependencyReportReader
                                               as X
import           YACP.Reader.ItDependsReader   as X
import           YACP.Reader.OrtEvaluatedModelReader
                                               as X
import           YACP.Reader.ScancodeReader    as X
import           YACP.Reader.ScanossReader     as X
import           YACP.Reader.SyftReader        as X
import           YACP.Writer.CSVWriter         as X
import           YACP.Writer.StateWriter       as X

import qualified Control.Monad.State           as MTL
import qualified Data.ByteString.Lazy          as B
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                )
import           System.Environment             ( getArgs )
import           System.Exit
import           System.IO

failOnIssue :: YACP ()
failOnIssue = do
  issues <- MTL.gets (_getYacpIssues)
  case issues of
    [] -> pure ()
    is -> do
      stderrLog ("Failed with " ++ (show (length is)) ++ " issues")
      MTL.liftIO exitFailure

argsToYACP' :: [String] -> YACP ()
argsToYACP' []       = return ()
argsToYACP' [outDir] = do
  -- ppState
  MTL.liftIO $ createDirectoryIfMissing True outDir
  writeStateFile (outDir </> "_state.json")
  writeCSVFile (outDir </> "out.csv")
  -- writePlantumlFile (outDir </> "plantuml.puml")
  -- _ <- writeDigraphFile (outDir </> "digraph.dot")
  -- writeHHCFile (outDir </> "hhc.json")
  failOnIssue
argsToYACP' ("--component-detection" : (f : oArgs)) =
  readComponentDetectionFile f >> argsToYACP' oArgs
argsToYACP' ("--fosslight" : (f : oArgs)) =
  readFosslightDepRepFile f >> argsToYACP' oArgs
argsToYACP' ("--it-depends" : (f : oArgs)) =
  readItDependsFile f >> argsToYACP' oArgs
argsToYACP' ("--ort" : (f : oArgs)) =
  readEvaluatedModelFile f >> argsToYACP' oArgs
argsToYACP' ("--scancode" : (f : oArgs)) =
  readScancodeFile f >> argsToYACP' oArgs
argsToYACP' ("--syft" : (f : oArgs)) = readSyftFile f >> argsToYACP' oArgs
-- argsToYACP' ("--spdx": (f: oArgs)) = parseSPDXFile f >> argsToYACP' oArgs
-- argsToYACP' ("--hhc": (f: oArgs)) = parseHHCFile f >> argsToYACP' oArgs
argsToYACP' (unknown  : oArgs      ) = MTL.liftIO $ do
  putStrLn ("failed to parse: " ++ unknown)
  exitFailure


argsToYACP :: [String] -> IO ()
argsToYACP =
  let help :: IO ()
      help = putStrLn $ unlines
        [ "yacp - yet another compliance platform"
        , "$0 $inputs $outputFolder"
        , "  where $inputs are multiples of"
        -- , "    --sc $scancode_file <- parse scancode file"
        -- , "    --ort $ort_file <- parse ort file"
        -- , "    --spdx $spdx_file <- parse spdx json file"
        , "  and $outputFolder <- dir to write files to"
        , "$0 [-h|--help] <- show this msg"
        ]
  in  \case
        []         -> help
        ["-h"    ] -> help
        ["--help"] -> help
        args       -> do
          (_, state) <- runYACP (argsToYACP' args)
          return ()
