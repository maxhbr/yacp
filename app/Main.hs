module Main where

import YACP
import System.IO

main :: IO ()
main = let
  yacp = do
    parseOrtFile "test/data/analyzer-result.json"
    parseScancodeFile "test/data/bat.scancode.pp.json"
    parseScancodeFile "test/data/black.scancode.pp.json"
    ppState
    writePlantumlFile "out.puml"
  in do
  (_, state) <- runYACP yacp
  return ()
