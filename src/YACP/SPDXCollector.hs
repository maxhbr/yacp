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

convertSPDXFile :: SPDXFile -> File
convertSPDXFile spdxFile = let
  in File
  undefined -- _getFileRootIdentifier :: Identifier
  undefined -- _getFilePath :: FilePath
  undefined
  undefined -- _getFileOtherIdentifier :: Identifier
  undefined -- _getFileLicense :: Maybe SPDX.LicenseExpression

convertSPDXPackage :: SPDXPackage -> Component
convertSPDXPackage spdxPackage = let
    identifier = let
        packageVerificationCode =
          map (Identifier . _SPDXPackageVerificationCode_packageVerificationCodeValue)
          (maybeToList (_SPDXPackage_packageVerificationCode spdxPackage))
        checksums =
          map (\(SPDXChecksum algo value) -> Hash (Just (show algo)) value)
          (_SPDXPackage_checksums spdxPackage)
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
