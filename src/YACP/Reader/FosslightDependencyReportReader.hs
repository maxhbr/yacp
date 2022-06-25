{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.FosslightDependencyReportReader
  ( readFosslightDepRepFile
  , readFosslightDepRepBS
  -- for testing:
  , parseFosslightDepRepBS
  ) where

import           YACP.Core

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import qualified Data.ByteString.Lazy          as B
import           Data.List                      ( intercalate )
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                )
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Distribution.Parsec           as SPDX
import qualified Distribution.SPDX             as SPDX
import qualified System.FilePath               as FP
import Data.Char (ord)
import qualified Data.Csv as Csv


data FDRRow = FDRRow String -- ID
                     FilePath -- Source Name or Path
                     String -- OSS Name
                     String -- OSS Version
                     (Maybe String) -- License
                     (Maybe String) -- Download Location
                     (Maybe String) -- Homepage
                     (Maybe String) -- Copyright Text
                     (Maybe String) -- Exclude
                     (Maybe String) -- Comment
                     deriving (Eq, Show)
instance Csv.FromNamedRecord FDRRow where
    parseNamedRecord m = FDRRow <$> m Csv..: "ID"
                                <*> m Csv..: "Source Name or Path"
                                <*> m Csv..: "OSS Name"
                                <*> m Csv..: "OSS Version"
                                <*> m Csv..: "License"
                                <*> m Csv..: "Download Location"
                                <*> m Csv..: "Homepage"
                                <*> m Csv..: "Copyright Text"
                                <*> m Csv..: "Exclude"
                                <*> m Csv..: "Comment"

parseFosslightDepRepBS :: B.ByteString -> Either YACPIssue (V.Vector FDRRow)
parseFosslightDepRepBS bs = let
    tsvOptions = Csv.defaultDecodeOptions {
        Csv.decDelimiter = fromIntegral (ord '\t')
        }
  in case Csv.decodeByNameWith tsvOptions bs of
    Right (_, rs) -> Right rs
    Left err -> Left (YACPParsingIssue err)

rowToStatements :: FDRRow -> Statements
rowToStatements (FDRRow _ src name version lic download home copyright exclude comment) = let
      identifier = nameAndVersion name version
      statementMetadata = StatementMetadata identifier Nothing
    in mconcat [ packStatements statementMetadata [ (FoundManifestFile (AbsolutePathIdentifier src))]
               , packStatements statementMetadata (map ComponentUrl $ Maybe.maybeToList download)
               , packStatements statementMetadata (map ComponentUrl $ Maybe.maybeToList home)
               , packStatements statementMetadata (map (ComponentLicense . String.fromString) $ Maybe.maybeToList lic)
               ]

readFosslightDepRepBS :: B.ByteString -> YACP (Maybe YACPIssue)
readFosslightDepRepBS bs = case parseFosslightDepRepBS bs of
  Right rs -> do
    let stmtss = V.map rowToStatements rs
    V.mapM_ addStatements stmtss
    return Nothing
  Left issue -> return (Just issue)

readFosslightDepRepFile :: FilePath -> YACP ()
readFosslightDepRepFile = readBSFromFile readFosslightDepRepBS