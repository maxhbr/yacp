{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.SPDXCollector
  ( SPDXDocument (..)
  , parseSPDXBS
  ) where

import YACP.Core
import YACP.ParserHelper
import YACP.SPDX.SPDX

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List (intercalate)
import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.FilePath as FP
import qualified Control.Monad.State as MTL
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec as SPDX

spdxidToIdentifier :: SPDXID -> Identifier
spdxidToIdentifier = Identifier

checksumsToIdentifier :: [SPDXChecksum] -> Identifier
checksumsToIdentifier = mconcat . map (\(SPDXChecksum algo value) -> Hash (Just (show algo)) value)

  -- { _SPDXFile_SPDXID :: SPDXID
  -- , _SPDXFile_raw :: A.Object
  -- , _SPDXFile_fileName :: String
  -- , _SPDXFile_fileTypes :: Maybe [SPDXFileType]
  -- , _SPDXFile_checksums :: [SPDXChecksum]
  -- , _SPDXFile_licenseInfoInFiles :: [String]
  -- , _SPDXFile_licenseInfoFromFiles :: Maybe [String]
  -- , _SPDXFile_licenseComments :: Maybe String
  -- , _SPDXFile_copyrightText :: String
  -- , _SPDXFile_comment :: Maybe String
  -- , _SPDXFile_noticeText :: Maybe String
  -- , _SPDXFile_fileContributors :: Maybe [String]
  -- , _SPDXFile_attributionTexts :: Maybe [String]

  -- , _SPDXFile_fileDependencies :: Maybe [SPDXID]
  -- -- , _SPDXFile_annotations ::
  -- -- , _SPDXFile_artifactOfs ::???
  -- , _SPDXFile_name :: Maybe String
  -- } deriving (Eq, Show)
convertSPDXFile :: SPDXFile -> File
convertSPDXFile spdxFile = let
  filetype = FileType_File -- TODO
  licenseExpression = case _SPDXFile_licenseInfoInFiles spdxFile of
    [] -> Nothing
    liifs -> undefined
  in File
  defaultFileRootIdentifier -- TODO
  (_SPDXFile_fileName spdxFile)
  filetype
  (checksumsToIdentifier (_SPDXFile_checksums spdxFile))
  licenseExpression

convertSPDXPackage :: SPDXPackage -> Component
convertSPDXPackage spdxPackage = let
    identifier = let
        packageVerificationCode =
          map (Identifier . _SPDXPackageVerificationCode_packageVerificationCodeValue)
          (maybeToList (_SPDXPackage_packageVerificationCode spdxPackage))
        checksums = [checksumsToIdentifier (_SPDXPackage_checksums spdxPackage)]
      in Identifiers (packageVerificationCode ++ checksums)
    componentLicense = spdxMaybeToMaybe (_SPDXPackage_licenseConcluded spdxPackage)
  in Component
  identifier
  componentLicense
  (V.singleton (A.Object (_SPDXPackage_raw spdxPackage)))
  [] -- _getComponentRelations :: [Relation]
  [] -- _getComponentSubComponents :: [Component]
convertSPDXRelationship :: SPDXRelationship -> Relation
convertSPDXRelationship (SPDXRelationship _ t target src)
  = Relation (spdxidToIdentifier src) t (spdxidToIdentifier target)

parseSPDXBS :: B.ByteString -> YACP ()
parseSPDXBS bs =
  case (A.eitherDecode bs :: Either String SPDXDocument) of
    Right spdx -> do
      addRoots (V.fromList (map spdxidToIdentifier (_SPDX_documentDescribes spdx)))
      addFiles (V.fromList (map convertSPDXFile (_SPDX_files spdx)))
      addComponents (V.fromList (map convertSPDXPackage (_SPDX_packages spdx)))
      addRelations (V.fromList (map convertSPDXRelationship (_SPDX_relationships spdx)))
    Left err   -> stderrLog err
