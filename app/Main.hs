module Main where

import YACP
import System.IO

main :: IO ()
main = let
  yacp = parseOrtFile "test/data/scanner_result.json"
  in do
  (_,result) <- runYACP yacp
  withFile "out.puml" WriteMode $ \h ->
    writePlantuml h result
