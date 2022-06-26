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
data EvaluatedModelPackage
  = EvaluatedModelPackage
  deriving (Eq, Show)

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
data EvaluatedModelFile
  = EvaluatedModelFile 
  deriving (Eq, Show)
instance A.FromJSON EvaluatedModelFile where
    parseJSON = A.withObject "EvaluatedModelFile" $ \v -> return EvaluatedModelFile

readEvaluatedModelFile = undefined
readEvaluatedModelBS = undefined
parseEvaluatedModelBS = undefined