module Main where

import YACP

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= argsToYACP