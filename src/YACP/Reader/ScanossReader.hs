{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.ScanossReader
  ( readScanossFile
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
import qualified Distribution.Parsec           as SPDX
import qualified Distribution.SPDX             as SPDX
import qualified System.FilePath               as FP


type ScanossFindingLines = String

-- {
--   "name": "Apache-2.0",
--   "obligations": "https://www.osadl.org/fileadmin/checklists/unreflicenses/Apache-2.0.txt",
--   "copyleft": "no",
--   "patent_hints": "yes",
--   "source": "component_declared"
-- }
data ScanossFindingLicense = ScanossFindingLicense
  { _SCF_L_name   :: String
  , _SCF_L_source :: String
  }
  deriving (Eq, Show)
instance A.FromJSON ScanossFindingLicense where
  parseJSON = A.withObject "ScanossFindingLicense"
    $ \v -> ScanossFindingLicense <$> v A..: "name" <*> v A..: "source"


-- {
--   "score": "4/5",
--   "source": "best_practices"
-- }
data ScanossFindingQuality = ScanossFindingQuality
  { _SCF_Q_score  :: String
  , _SCF_Q_source :: String
  }
  deriving (Eq, Show)
instance A.FromJSON ScanossFindingQuality where
  parseJSON = A.withObject "ScanossFindingQuality"
    $ \v -> ScanossFindingQuality <$> v A..: "score" <*> v A..: "source"

--  "src/server/testUtils/startTestServerWithConfig.ts": [
--    {
--      "id": "file",
--      "status": "pending",
--      "lines": "all",
--      "oss_lines": "all",
--      "matched": "100%",
--      "purl": [
--        "pkg:github/tng/virtual-office"
--      ],
--      "vendor": "TNG",
--      "component": "virtual-office",
--      "version": "57672f3",
--      "latest": "f22ae35",
--      "url": "https://github.com/tng/virtual-office",
--      "release_date": "2021-05-20",
--      "file": "virtual-office-57672f3681f0c77a488b560dbaa695bc20911174/server/testUtils/startTestServerWithConfig.ts",
--      "url_hash": "5f74139e38ccfadb85a366a0dea3f407",
--      "file_hash": "298e786ce207cb3ec1776b59682f9ffa",
--      "file_url": "https://osskb.org/api/file_contents/298e786ce207cb3ec1776b59682f9ffa",
--      "dependencies": [],
--      "licenses": [
--        {
--          "name": "Apache-2.0",
--          "obligations": "https://www.osadl.org/fileadmin/checklists/unreflicenses/Apache-2.0.txt",
--          "copyleft": "no",
--          "patent_hints": "yes",
--          "source": "component_declared"
--        }
--      ],
--      "copyrights": [],
--      "vulnerabilities": [],
--      "quality": [
--        {
--          "score": "4/5",
--          "source": "best_practices"
--        }
--      ],
--      "cryptography": [],
--      "server": {
--        "hostname": "p9",
--        "version": "4.2.4",
--        "flags": "0",
--        "elapsed": "0.003679s"
--      }
--    }
--  ]
data ScanossFinding = ScanossFinding
  { _SCF_id              :: String
  , _SCF_status          :: String
  , _SCF_lines           :: ScanossFindingLines
  , _SCF_osslines        :: ScanossFindingLines
  , _SCF_matched         :: String
  , _SCF_purl            :: [Identifier]
  , _SCF_vendor          :: String
  , _SCF_component       :: String
  , _SCF_version         :: String
  , _SCF_latest          :: String
  , _SCF_url             :: String
  , _SCF_release_date    :: String
  , _SCF_file            :: FilePath
  , _SCF_url_hash        :: String
  , _SCF_file_hash       :: String
  , _SCF_file_url        :: String
                  -- , _SCF_dependencies :: []
  , _SCF_licenses        :: [ScanossFindingLicense]
  , _SCF_copyrights      :: [String]
  , _SCF_vulnerabilities :: [String]
  , _SCF_quality         :: [ScanossFindingQuality]
  , _SCF_cryptography    :: [String]
                  -- , _SCF_server :: []
  }
  deriving (Eq, Show)
instance A.FromJSON ScanossFinding where
  parseJSON = A.withObject "ScanossFinding" $ \v -> undefined

data ScanossFile = ScanossFile (Map.Map FilePath [ScanossFinding])

parseScanossBS = undefined

readScanossFile = undefined
