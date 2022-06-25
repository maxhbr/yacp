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

import           PURL.PURL               hiding ( parsePURL )
import qualified PURL.PURL                     as PURL
import           SPDX.Document.RelationshipTypes
                                               as X
import           SPDX.LicenseExpression        as X

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import           Data.Dynamic
import           Data.List                      ( nub )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , maybeToList
                                                )
import qualified Data.Monoid                    ( mconcat )
import           Data.String                    ( IsString(..) )
import qualified Data.Text                     as T
import           Data.Typeable
import           Data.UUID                      ( UUID )
import qualified Data.Vector                   as V
import qualified Distribution.Parsec           as SPDX
import qualified Distribution.SPDX             as SPDX
import qualified Distribution.SPDX.Extra       as SPDX
import qualified Distribution.SPDX.License     as SPDX
import qualified Network.URI                   as URI
import           System.Console.Pretty          ( Color(Green)
                                                , color
                                                )
import qualified System.FilePath               as FP
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.Random                  ( randomIO )


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
  deriving (Eq, Show, Generic)
instance A.ToJSON ComponentLicense
instance A.FromJSON ComponentLicense
instance Statemental ComponentLicense
instance IdentifierProvider ComponentLicense

data ComponentUrl = ComponentUrl String
  deriving (Eq, Show, Generic)
instance A.ToJSON ComponentUrl
instance A.FromJSON ComponentUrl
instance Statemental ComponentUrl
instance IdentifierProvider ComponentUrl

data ComponentVulnerability = ComponentVulnerability String
  deriving (Eq, Show, Generic)
instance A.ToJSON ComponentVulnerability
instance A.FromJSON ComponentVulnerability
instance Statemental ComponentVulnerability
instance IdentifierProvider ComponentVulnerability
