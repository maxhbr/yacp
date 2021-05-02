{-# LANGUAGE LambdaCase #-}
module Main where

import YACP.HHC.HHC
import YACP.HHC.HHCUtils

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified System.FilePath as FP
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8

parseHHC :: FP.FilePath -> IO HHC
parseHHC fp = do
  hPutStrLn stderr ("parse: " ++ fp)
  bs <- B.readFile fp
  case (A.eitherDecode' bs) of
    Right hhc -> return hhc
    Left err  -> do
        hPutStrLn stderr err
        undefined


main :: IO ()
main = do
    args <- getArgs
    hhcs <- mapM parseHHC args
    let finalHHC = clusterifyHHC $ mconcat hhcs
    C8.putStrLn (A.encodePretty finalHHC)
