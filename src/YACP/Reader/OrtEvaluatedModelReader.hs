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
import qualified Data.Maybe as Maybe

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
  , _evaluatedModelPackagePurl :: Maybe PURL
  , _evaluatedModelPackageLevels :: [Int]
  , _evaluatedModelPackageScopes :: [String]
  , _evaluatedModelPackageDetectedLicenses :: [MaybeLicenseExpression]
  , _evaluatedModelPackageDeclaredLicenses :: MaybeLicenseExpression
  , _evaluatedModelPackageDefinitionFilePath :: Maybe FilePath
  } deriving (Eq, Show)

data EvaluatedModelDependencyTree
  = EvaluatedModelDependencyTree
  { _evaluatedModelDependencyTreeKey :: Int
  , _evaluatedModelDependencyTreeSubject :: Maybe Identifier
  , _evaluatedModelDependencyTreeChildren :: [EvaluatedModelDependencyTree]
  , _evaluatedModelDependencyTreeScope :: Maybe String
  , _evaluatedModelDependencyTreeLinkage :: Maybe String
  } deriving (Eq, Show)

data EvaluatedModelFile
  = EvaluatedModelFile
  { _evaluatedModelLicenses :: (Map.Map Int MaybeLicenseExpression)
  , _evaluatedModelScopes :: (Map.Map Int String)
  , _evaluatedModelCopyrights :: (Map.Map Int String)
  , _evaluatedModelPackages :: (Map.Map Int EvaluatedModelPackage)
  , _evaluatedModelDependencyTrees :: [EvaluatedModelDependencyTree]
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

      lookupsInMap :: Map.Map Int a -> [Int] -> [a]
      lookupsInMap m is = Maybe.mapMaybe (`Map.lookup` m) is

      parsePackage :: (Map.Map Int MaybeLicenseExpression) -> (Map.Map Int String) -> A.Object -> A.Parser EvaluatedModelPackage
      parsePackage licenseMap scopeMap = \v -> do
        EvaluatedModelPackage <$> v A..: "id"
                              <*> (v A..:? "purl" >>= return . \case
                                     Just str -> parsePURL str
                                     Nothing -> Nothing)
                              <*> v A..: "levels"
                              <*> (v A..:? "scopes" >>= return . \case
                                     Just ids -> lookupsInMap scopeMap ids
                                     Nothing -> [])
                              <*> (v A..: "detected_licenses" >>= return . lookupsInMap licenseMap)
                              <*> (v A..: "declared_licenses_processed" 
                                   >>= (\v' -> do
                                    spdxExp <- (v' A..:? "spdx_expression">>= return . \case
                                            Just exp -> fromString exp
                                            Nothing -> MLicExp NOASSERTION)
                                    unmapped <- (([] `fromMaybe`) <$> v' A..:? "unmapped_licenses") >>= return . lookupsInMap licenseMap
                                    return (mconcat (spdxExp:unmapped))))
                              <*> (v A..: "definition_file_path" >>= return . \case
                                     "" -> Nothing
                                     dfp -> Just dfp)
      parsePackageMap :: (Map.Map Int MaybeLicenseExpression) -> (Map.Map Int String) -> A.Value -> A.Parser (Map.Map Int EvaluatedModelPackage)
      parsePackageMap licenseMap scopeMap = parseToMap (parsePackage licenseMap scopeMap)

      parseDependencyTree :: (Map.Map Int String) -> (Map.Map Int EvaluatedModelPackage) -> A.Value -> A.Parser EvaluatedModelDependencyTree
      parseDependencyTree scopeMap packageMap = A.withObject "EvaluatedModelDependencyTree" $ \v ->
        EvaluatedModelDependencyTree <$> v A..: "key"
                                     <*> (v A..:? "pkg" >>= return . \case
                                            Just i -> case  i `Map.lookup` packageMap of
                                               Just p -> Just $ Identifier (_evaluatedModelPackageId p)
                                               Nothing -> Nothing
                                            Nothing -> Nothing)
                                     <*> (v A..:? "children" >>= \case 
                                       Just ts -> parseDependencyTrees scopeMap packageMap ts
                                       Nothing -> return [])
                                     <*> (v A..:? "scope" >>= return . \case
                                            Just i -> i `Map.lookup` scopeMap
                                            Nothing -> Nothing)
                                     <*> v A..:? "linkage"
      parseDependencyTrees :: (Map.Map Int String) -> (Map.Map Int EvaluatedModelPackage) -> A.Value -> A.Parser [EvaluatedModelDependencyTree]
      parseDependencyTrees scopeMap packageMap = A.withArray "Array" $ \v -> mapM (parseDependencyTree scopeMap packageMap) (V.toList v)

    in A.withObject "EvaluatedModelFile" $ \v -> do
      licenseMap <- (v A..: "licenses" >>= parseSimpleToMap "id") :: A.Parser (Map.Map Int MaybeLicenseExpression)
      scopeMap <- (v A..: "scopes" >>= parseSimpleToMap "name") :: A.Parser (Map.Map Int String)
      copyrightMap <- (v A..: "copyrights" >>= parseSimpleToMap "statement") :: A.Parser (Map.Map Int String)

      packageMap <- (v A..: "packages" >>= parsePackageMap licenseMap scopeMap) :: A.Parser (Map.Map Int EvaluatedModelPackage)

      dependencyTrees <- (v A..: "dependency_trees" >>= parseDependencyTrees scopeMap packageMap) :: A.Parser [EvaluatedModelDependencyTree]

      return (EvaluatedModelFile licenseMap scopeMap copyrightMap packageMap dependencyTrees)

convertEvaluatedModel :: EvaluatedModelFile -> Statements
convertEvaluatedModel (EvaluatedModelFile {_evaluatedModelPackages = packages}) = let
    convertEvaluatedPackage :: EvaluatedModelPackage -> [Statement]
    convertEvaluatedPackage p = let
        identifier = Identifier (_evaluatedModelPackageId p) <> (case _evaluatedModelPackagePurl p of
          Just p -> PurlIdentifier p
          Nothing -> mempty)
        detectedLicenses = Statement identifier (DetectedLicenses (_evaluatedModelPackageDetectedLicenses p))
        declaredLicenses = Statement identifier (ComponentLicense (_evaluatedModelPackageDeclaredLicenses p))
        manifestFile = case _evaluatedModelPackageDefinitionFilePath p of
          Just manifest -> [Statement identifier (FoundManifestFile (AbsolutePathIdentifier manifest))]
          Nothing -> []

      in [declaredLicenses, detectedLicenses] ++ manifestFile
  in (Statements . V.fromList . concatMap convertEvaluatedPackage . Map.elems) packages

parseEvaluatedModelBS :: B.ByteString -> Either YACPIssue EvaluatedModelFile
parseEvaluatedModelBS bs = case A.eitherDecode bs of
  Right cd  -> Right cd
  Left  err -> Left (YACPParsingIssue err)

readEvaluatedModelBS :: Origin -> B.ByteString -> YACP (Maybe YACPIssue)
readEvaluatedModelBS o bs = case parseEvaluatedModelBS bs of
  Right file -> do
    let statements = setOrigin o $ convertEvaluatedModel file
    addStatements statements
    return Nothing
  Left issue -> return (Just issue)

readEvaluatedModelFile :: FilePath -> YACP ()
readEvaluatedModelFile f = readBSFromFile (readEvaluatedModelBS (OriginToolReport "ort" f)) f