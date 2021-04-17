module Main where

import YACP
import System.IO

main :: IO ()
main = let
  yacp = do
    parseOrtFile "test/data/analyzer-result.json"
    parseScancodeFile "test/data/bat.scancode.pp.json"
    parseScancodeFile "test/data/black.scancode.pp.json"
    parseScancodeFile "test/data/example.scancode.pp.json"
    parseSPDXFile "data/spdx-spdx-spec/examples/SPDXJSONExample-v2.2.spdx.json"
    ppState
    writePlantumlFile "_tmp/plantuml.puml"
    writeDigraphFile "_tmp/digraph.dot"
    writeHHCFile "_tmp/hhc.json"
  in do
  (_, state) <- runYACP yacp
  return ()
