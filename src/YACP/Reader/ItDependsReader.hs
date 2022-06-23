{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.ItDependsReader
  ( readItDependsFile
  , readItDependsBS
  -- for testing:
  , parseItDependsBS
  ) where

import           YACP.Core

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.KeyMap             as A
import qualified Data.Aeson.Types              as A
import qualified Data.ByteString.Lazy          as B
import qualified Data.Map                      as Map
import qualified Data.Vector                   as V
import qualified System.IO                     as IO

data ItDependsEntryDependency
    = ItDependsEntryDependency
    { _ided_name :: String
    , _ided_version :: String
    } deriving (Eq, Show)

data ItDependsMetadata 
    = ItDependsMetadata 
    { _idev_dependencies :: Map.Map String String
    , _idev_vulnerabilities :: [String]
    } deriving (Eq, Show)
instance A.FromJSON ItDependsMetadata where
  parseJSON = A.withObject "ItDependsMetadata" $ \v ->
    ItDependsMetadata <$> v A..: "dependencies"
                      <*> v A..: "vulnerabilities"

data ItDependsFile
    = ItDependsFile (Map.Map String (Map.Map String ItDependsMetadata))
    deriving (Eq, Show)
instance A.FromJSON ItDependsFile where
  parseJSON a = do
    parsed <- A.parseJSON a
    return (ItDependsFile parsed)

convertItDepends :: ItDependsFile -> Statements
convertItDepends (ItDependsFile m) = Statements . V.fromList $ concatMap (\(name, m') -> 
        concatMap (\(version, ItDependsMetadata dependencies vulnerabilities) -> let
                identifier = nameAndVersion name version
                statementMetadata = StatementMetadata identifier Nothing
            in map (Statement statementMetadata) $
                ((map (FoundDependency . (\(name,version) -> nameAndVersion name version)) (Map.toList dependencies)))
                ++ (map ComponentVulnerability vulnerabilities) 
        ) (Map.toList m')
    ) (Map.toList m)

parseItDependsBS
  :: B.ByteString -> Either YACPIssue ItDependsFile
parseItDependsBS bs = case A.eitherDecode bs of
  Right cd  -> Right cd
  Left  err -> Left (YACPParsingIssue err)

readItDependsBS :: B.ByteString -> YACP (Maybe YACPIssue)
readItDependsBS bs = case parseItDependsBS bs of
  Right file -> do
    let statements = convertItDepends file
    addStatements statements
    return (Nothing)
  Left issue -> return (Just issue)

readItDependsFile :: FilePath -> YACP ()
readItDependsFile = readBSFromFile readItDependsBS
