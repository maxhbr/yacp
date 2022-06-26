{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}
module YACP.Core.StatementContents where

import           YACP.Core.Model
import           YACP.Core.MyPrelude

import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A

data FoundDependent = FoundDependent Identifier
  deriving (Eq, Show, Generic)
instance A.ToJSON FoundDependent
instance A.FromJSON FoundDependent
instance IdentifierProvider FoundDependent where
  getIdentifiers (FoundDependent i) = pure i
instance Statemental FoundDependent

data FoundDependency = FoundDependency Identifier
  deriving (Eq, Show, Generic)
instance A.ToJSON FoundDependency
instance A.FromJSON FoundDependency
instance Statemental FoundDependency
instance IdentifierProvider FoundDependency where
  getIdentifiers (FoundDependency i) = pure i

data FoundManifestFile = FoundManifestFile Identifier
  deriving (Eq, Show, Generic)
instance A.ToJSON FoundManifestFile
instance A.FromJSON FoundManifestFile
instance Statemental FoundManifestFile
instance IdentifierProvider FoundManifestFile where
  getIdentifiers (FoundManifestFile i) = pure i

data ComponentLicense = ComponentLicense MaybeLicenseExpression
  deriving (Eq, Generic)
instance Show ComponentLicense where
  show (ComponentLicense l) = show l
instance A.ToJSON ComponentLicense
instance A.FromJSON ComponentLicense
instance Statemental ComponentLicense where
  isEmpty (ComponentLicense (MLicExp NOASSERTION)) = True
  isEmpty _ = False
instance IdentifierProvider ComponentLicense

data DetectedLicenses = DetectedLicenses [MaybeLicenseExpression]
  deriving (Eq, Generic)
instance Show DetectedLicenses where
  show (DetectedLicenses l) = show l
instance A.ToJSON DetectedLicenses
instance A.FromJSON DetectedLicenses
instance Statemental DetectedLicenses where
  isEmpty (DetectedLicenses ls) = null (filter (/= (MLicExp NOASSERTION)) ls)
instance IdentifierProvider DetectedLicenses

data ComponentUrl = ComponentUrl String
  deriving (Eq, Generic)
instance Show ComponentUrl where
  show (ComponentUrl url) = url
instance A.ToJSON ComponentUrl
instance A.FromJSON ComponentUrl
instance Statemental ComponentUrl
instance IdentifierProvider ComponentUrl

data ComponentVulnerability = ComponentVulnerability String
  deriving (Eq, Generic)
instance Show ComponentVulnerability where
  show (ComponentVulnerability v) = v
instance A.ToJSON ComponentVulnerability
instance A.FromJSON ComponentVulnerability
instance Statemental ComponentVulnerability
instance IdentifierProvider ComponentVulnerability
