{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Writer.CSVWriter
  ( writeCSVFile
  ) where

import           YACP.Core

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.Aeson.Types              as A
import qualified Data.ByteString.Lazy          as B
import qualified Data.Csv                      as Csv
import qualified Data.Vector                   as V
import           System.IO                      ( Handle
                                                , hClose
                                                , hPutStrLn
                                                , stdout
                                                )
import qualified System.IO                     as IO

data CSVRecord = CSVRecord Identifier

instance Csv.ToNamedRecord CSVRecord where
  toNamedRecord (CSVRecord identifier) =
    Csv.namedRecord ["Identifier" Csv..= show identifier]

csvRecordHeader :: Csv.Header
csvRecordHeader = V.fromList ["Identifier"]

genCSV :: State -> V.Vector CSVRecord
genCSV (State { _getStatements = sts }) =
  let clusters = clusterifyStatements sts
  in  V.map (\(i, sts') -> CSVRecord i) clusters

writeCSV :: Handle -> YACP ()
writeCSV h = MTL.get >>= \state -> do
  let csv = genCSV state
  MTL.liftIO $ do
    B.hPutStr h ((Csv.encodeByName csvRecordHeader . V.toList) csv)

writeCSVFile :: FilePath -> YACP ()
writeCSVFile fp = do
  stderrLog $ "csv " ++ fp
  state <- MTL.get
  MTL.liftIO $ do
    IO.withFile fp IO.WriteMode $ \h -> runYACP' (writeCSV h) state
  return ()
