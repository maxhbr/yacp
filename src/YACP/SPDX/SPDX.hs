{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.SPDX.SPDX
  ( module X
  , SPDXDocument (..)
  ) where

import YACP.Core
import YACP.ParserHelper

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Text as T

import YACP.SPDX.Common as X
import YACP.SPDX.DocumentCreationInformation as X
import YACP.SPDX.PackageInformation as X
import YACP.SPDX.FileInformation as X
import YACP.SPDX.SnippetInformation as X
import YACP.SPDX.OtherLicensingInformationDetected as X
import YACP.SPDX.Annotations as X
import YACP.SPDX.RelationshipsbetweenSPDXElements as X

data SPDXDocument
  = SPDXDocument
  { _SPDX_SPDXID :: SPDXID
  , _SPDX_comment :: Maybe String
  , _SPDX_spdxVersion :: String
  , _SPDX_creationInfo :: SPDXCreationInfo
  , _SPDX_name :: String
  , _SPDX_dataLicense :: String
  , _SPDX_documentDescribes :: [SPDXID]
  -- , _SPDX_describesPackages :: [SPDXID]

  , _SPDX_files :: [SPDXFile]
  , _SPDX_packages :: [SPDXPackage]
  , _SPDX_relationships :: [SPDXRelationship]
  } deriving (Eq, Show)
instance A.FromJSON SPDXDocument where
  parseJSON = A.withObject "SPDXDocument" $ \v ->
    SPDXDocument
    <$> v A..: "SPDXID"
    <*> v A..:? "comment"
    <*> v A..: "spdxVersion"
    -- <*> v A..: "externalDocumentRefs"
    <*> v A..: "creationInfo"
    <*> v A..: "name"
    <*> v A..: "dataLicense"
    <*> v A..: "documentDescribes" -- or "describesPackages"

    <*> v A..: "files"
    <*> v A..: "packages"
    -- <*> v A..: "hasExtractedLicensingInfos"
    -- <*> v A..: "snippets"
    <*> v A..: "relationships"
    -- <*> v A..: "revieweds"
    -- <*> v A..: "annotations"
