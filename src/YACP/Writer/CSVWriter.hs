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
                           [(ComponentLicense, Origin)]
                           [(DetectedLicenses, Origin)]
                           [(ComponentVulnerability, Origin)]
                           [Origin]

instance Csv.ToNamedRecord CSVRecord where
  toNamedRecord (CSVRecord identifier lics detLics vulns origins) =
    let toListOfStrings :: Show a => [(a, Origin)] -> String
        toListOfStrings =
          let fun :: Show a => (a, Origin) -> String
              fun (a, o) = show a ++ " via " ++ show o
          in  unlines . map fun
    in  Csv.namedRecord
          [ "Identifier"
            Csv..= (unlines . map show . flattenIdentifierToList) identifier
          , "Licenses" Csv..= toListOfStrings lics
          , "DetectedLicenses" Csv..= toListOfStrings detLics
          , "Vulnerabilities" Csv..= toListOfStrings vulns
          , "Origins" Csv..= (unlines . map show) origins
          ]

csvRecordHeader :: Csv.Header
csvRecordHeader = V.fromList
  ["Identifier", "Licenses", "DetectedLicenses", "Vulnerabilities", "Origins"]

genCSV :: State -> V.Vector CSVRecord
genCSV (State { _getStatements = sts }) =
  let clusters = clusterifyStatements sts
  in  V.map
        (\(i, sts') ->
          let lics    = unpackStatements sts'
              detLics = unpackStatements sts'
              vulns   = unpackStatements sts'
              origins = getOrigins sts'
          in  CSVRecord i lics detLics vulns origins
        )
        clusters

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
