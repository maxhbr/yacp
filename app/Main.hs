{-# LANGUAGE LambdaCase #-}
module Main where

import YACP

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as C8
import YACP.HHC.HHCUtils ( computeMergedHHC, parseSpdxToHHC )

main :: IO ()
main = getArgs >>= \case
    "--merge-hhcs":args -> computeMergedHHC args >>= C8.putStrLn
    "--spdx-to-hhc":[spdx] -> parseSpdxToHHC spdx >>= C8.putStrLn
    _ -> argsToYACP
