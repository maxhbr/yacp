{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.SyftReader
  ( readSyftFile
  , readSyftBS
  -- for testing:
  , parseSyftBS
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

{-
Schema: https://raw.githubusercontent.com/anchore/syft/main/schema/json/schema-3.3.0.json
 -}

data SyftArtifact = SyftArtifact
  { _syft_id        :: String
  , _syft_name      :: String
  , _syft_version   :: String
  , _syft_locations :: [FilePath]
  , _syft_licenses  :: [MaybeLicenseExpression]
  , _syft_cpes      :: [String]
  , _syft_purl      :: Maybe PURL
  }
  deriving (Eq, Show)
instance A.FromJSON SyftArtifact where
  parseJSON = A.withObject "SyftArtifact" $ \v ->
    SyftArtifact
      <$>  v
      A..: "id"
      <*>  v
      A..: "name"
      <*>  v
      A..: "version"
      <*>  (v A..: "locations" >>= mapM (\v' -> v' A..: "path"))
      <*>  v
      A..: "licenses"
      <*>  v
      A..: "cpes"
      <*>  (v A..: "purl" >>= return . parsePURL)


data SyftFile = SyftFile [SyftArtifact]
  deriving (Eq, Show)
instance A.FromJSON SyftFile where
  parseJSON = A.withObject "SyftFile" $ \v -> SyftFile <$> v A..: "artifacts"

convertSyft :: SyftFile -> Statements
convertSyft (SyftFile artifacts) =
  let
    convertArtifact :: SyftArtifact -> Statements
    convertArtifact a =
      let identifier = mconcat
            [ (case _syft_purl a of
                Just p  -> PurlIdentifier p
                Nothing -> mempty
              )
            , nameAndVersion (_syft_name a) (_syft_version a)
            , Identifier (_syft_id a)
            ]
      in
        (Statements . V.fromList)
          ( ((Statement identifier . ComponentLicense . mconcat)
              (_syft_licenses a)
            )
          : (map
              ( Statement identifier
              . FoundManifestFile
              . AbsolutePathIdentifier
              )
              (_syft_locations a)
            )
          )
  in
    mconcat $ map convertArtifact artifacts

parseSyftBS :: B.ByteString -> Either YACPIssue SyftFile
parseSyftBS bs = case A.eitherDecode bs of
  Right cd  -> Right cd
  Left  err -> Left (YACPParsingIssue err)

readSyftBS :: Origin -> B.ByteString -> YACP (Maybe YACPIssue)
readSyftBS o bs = case parseSyftBS bs of
  Right file -> do
    let statements = setOrigin o $ convertSyft file
    addStatements statements
    return Nothing
  Left issue -> return (Just issue)

readSyftFile :: FilePath -> YACP ()
readSyftFile f = readBSFromFile (readSyftBS (OriginToolReport "syft" f)) f

