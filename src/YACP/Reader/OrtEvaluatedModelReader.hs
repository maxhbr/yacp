{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.OrtEvaluatedModelReader
  ( readEvaluatedModelFile
  , readEvaluatedModelBS
  -- for testing:
  , parseEvaluatedModelBS
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
$ cat test/data/ort/evaluated-model.json | jq '.packages[3] | keys'
[
  "_id",
  "binary_artifact",
  "declared_licenses_processed",
  "definition_file_path",
  "detected_licenses",
  "findings",
  "id",
  "is_excluded",
  "is_project",
  "levels",
  "paths",
  "purl",
  "scan_results",
  "scopes",
  "source_artifact",
  "vcs",
  "vcs_processed"
]
-}

{-
$ cat test/data/ort/evaluated-model.json | jq 'keys'
[
  "copyrights",
  "dependency_trees",
  "issue_resolutions",
  "issues",
  "labels",
  "licenses",
  "meta_data",
  "packages",
  "path_excludes",
  "paths",
  "repository",
  "repository_configuration",
  "rule_violation_resolutions",
  "rule_violations",
  "scan_results",
  "scope_excludes",
  "scopes",
  "severe_issue_threshold",
  "severe_rule_violation_threshold",
  "statistics",
  "vulnerabilities",
  "vulnerabilities_resolutions"
]
-}



data EvaluatedModelPackage
  = EvaluatedModelPackage
  { _evaluatedModelPackageId :: String
  } deriving (Eq, Show)

data EvaluatedModelFile
  = EvaluatedModelFile
  { _evaluatedModelLicenses :: (Map.Map Int MaybeLicenseExpression)
  , _evaluatedModelScopes :: (Map.Map Int String)
  , _evaluatedModelCopyrights :: (Map.Map Int String)
  , _evaluatedModelPackages :: (Map.Map Int EvaluatedModelPackage)
  }
  deriving (Eq, Show)
instance A.FromJSON EvaluatedModelFile where
  parseJSON = let
      parseToMap :: (A.Object -> A.Parser a) -> A.Value -> A.Parser (Map.Map Int a)
      parseToMap parser = A.withArray "Array" $ \v -> do
        entries <- mapM (A.withObject "ArrayItem" $ \v' -> 
          (,) <$> v' A..: "_id" <*> parser v') (V.toList v)
        return $ Map.fromList entries
      parseSimpleToMap :: A.FromJSON a => A.Key -> A.Value -> A.Parser (Map.Map Int a)
      parseSimpleToMap key = parseToMap (\v -> v A..: key)

      parsePackage :: (Map.Map Int MaybeLicenseExpression) -> A.Object -> A.Parser EvaluatedModelPackage
      parsePackage licenseMap = \v -> do
        EvaluatedModelPackage <$> v A..: "id"
      parsePackageMap :: (Map.Map Int MaybeLicenseExpression) -> A.Value -> A.Parser (Map.Map Int EvaluatedModelPackage)
      parsePackageMap licenseMap = parseToMap (parsePackage licenseMap)
    in A.withObject "EvaluatedModelFile" $ \v -> do
      licenseMap <- (v A..: "licenses" >>= parseSimpleToMap "id") :: A.Parser (Map.Map Int MaybeLicenseExpression)
      scopeMap <- (v A..: "scopes" >>= parseSimpleToMap "name") :: A.Parser (Map.Map Int String)
      copyrightMap <- (v A..: "copyrights" >>= parseSimpleToMap "statement") :: A.Parser (Map.Map Int String)

      packageMap <- (v A..: "packages" >>= parsePackageMap licenseMap) :: A.Parser (Map.Map Int EvaluatedModelPackage)

      return (EvaluatedModelFile licenseMap scopeMap copyrightMap packageMap)

parseEvaluatedModelBS :: B.ByteString -> Either YACPIssue EvaluatedModelFile
parseEvaluatedModelBS bs = case A.eitherDecode bs of
  Right cd  -> Right cd
  Left  err -> Left (YACPParsingIssue err)
readEvaluatedModelBS :: Origin -> B.ByteString -> YACP (Maybe YACPIssue)
readEvaluatedModelBS = undefined
readEvaluatedModelFile :: FilePath -> YACP ()
readEvaluatedModelFile = undefined