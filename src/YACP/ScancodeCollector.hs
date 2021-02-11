{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.ScancodeCollector
  ( ScancodeFile (..), ScancodeFileEntry (..)
  , parseScancodeBS
  ) where

import YACP.Core
import YACP.ParserHelper

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

{-
    {
      "path": "Cargo.lock",
      "type": "file",
      "name": "Cargo.lock",
      "base_name": "Cargo",
      "extension": ".lock",
      "size": 35114,
      "date": "2021-01-18",
      "sha1": "c3f1ca217637c6edec185df11cfdddbba1d3cac3",
      "md5": "c78cc22b4e1d5e6dade34aeb592c73f9",
      "sha256": "c2a6153229d3aad680248b0cbedf9cbc3cbfff5586be360e5f7efbb8dceca8cc",
      "mime_type": "text/plain",
      "file_type": "ASCII text",
      "programming_language": null,
      "is_binary": false,
      "is_text": true,
      "is_archive": false,
      "is_media": false,
      "is_source": false,
      "is_script": false,
      "licenses": [],
      "license_expressions": [],
      "percentage_of_license_text": 0,
      "copyrights": [],
      "holders": [],
      "authors": [],
      "packages": [
        {
          "type": "cargo",
          "namespace": null,
          "name": null,
          "version": null,
          "qualifiers": {},
          "subpath": null,
          "primary_language": "Rust",
          "description": null,
          "release_date": null,
          "parties": [],
          "keywords": [],
          "homepage_url": null,
          "download_url": null,
          "size": null,
          "sha1": null,
          "md5": null,
          "sha256": null,
          "sha512": null,
          "bug_tracking_url": null,
          "code_view_url": null,
          "vcs_url": null,
          "copyright": null,
          "license_expression": null,
          "declared_license": null,
          "notice_text": null,
          "root_path": null,
          "dependencies": [
            {
              "purl": "pkg:crates/adler@0.2.3",
              "requirement": "0.2.3",
              "scope": "dependency",
              "is_runtime": true,
              "is_optional": false,
              "is_resolved": true
            },
            {
              "purl": "pkg:crates/aho-corasick@0.7.15",
              "requirement": "0.7.15",
              "scope": "dependency",
              "is_runtime": true,
              "is_optional": false,
              "is_resolved": true
            },
...
          ],
          "contains_source_code": null,
          "source_packages": [],
          "purl": null,
          "repository_homepage_url": null,
          "repository_download_url": null,
          "api_data_url": null
        }
-}
data ScancodeFileEntry
  = ScancodeFileEntry
  { _scfe_file :: File
  , _scfe_packages :: [Component]
  , _scfe_relations :: [Relation]
  } deriving (Eq, Show)
instance A.FromJSON ScancodeFileEntry where
  parseJSON = A.withObject "ScancodeFileEntry" $ \v -> let

    getHash v' alg = fmap (maybeToList . fmap (Hash (Just alg))) $ v' A..:? T.pack alg
    getPurl v' = fmap (maybeToList . fmap parsePURL) $ v' A..:? "purl"

    parsePackage :: Identifier -> A.Object -> A.Parser Component
    parsePackage parentId v' = do
      purl <- getPurl v'
      sha1 <- v' `getHash` "sha1"
      md5 <- v' `getHash` "md5"
      sha256 <- v' `getHash` "sha256"
      sha512 <- v' `getHash` "sha512"
      let ids = purl ++ sha1 ++ md5 ++ sha256 ++ sha512
          idFromHashes = case ids of
            [] -> parentId
            _ -> mconcat ids
      dependencies <- (v' A..:? "dependencies" >>= (\case
                                                       Just dependencies -> mapM (parsePackage parentId) dependencies
                                                       Nothing -> return [])) :: A.Parser [Component]
      let dependencyRels = map (\c -> let
                                   depId = getIdentifier c
                                   in Relation depId DEPENDENCY_OF idFromHashes) dependencies

      license <- v' A..:? "license_expressions" >>= (\case
                                                        Just lics -> return $ parseLicenses lics
                                                        Nothing -> return Nothing)

      return $ Component idFromHashes license (V.singleton (A.Object v')) dependencyRels dependencies

    in do
    path <- v A..: "path"
    sha1 <- v `getHash` "sha1"
    md5 <- v `getHash` "md5"
    sha256 <- v `getHash` "sha256"
    sha512 <- v `getHash` "sha512"
    let idFromHashes = mconcat $ sha1 ++ md5 ++ sha256 ++ sha512
    license <- v A..:? "license_expressions" >>= (\case
                                                     Just lics -> return $ parseLicenses lics
                                                     Nothing -> return Nothing)
    let file = File defaultFileRootIdentifier path idFromHashes license
    let idFromFile = getIdentifier file

    packages <- v A..: "packages"
    components <- mapM (parsePackage idFromFile) packages
    let relations = map (Relation idFromFile METAFILE_OF . getIdentifier) components
    return (ScancodeFileEntry file components relations)

data ScancodeFile
  = ScancodeFile
  { _scf_files :: [ScancodeFileEntry]
  } deriving (Eq, Show)
instance A.FromJSON ScancodeFile where
  parseJSON = A.withObject "ScancodeFile" $ \v -> do
    ScancodeFile <$> v A..: "files"

parseScancodeBS :: B.ByteString -> YACP ()
parseScancodeBS bs =
  case (A.eitherDecode bs :: Either String ScancodeFile) of
    Right (ScancodeFile scFiles) -> let
      files = V.fromList $ map _scfe_file scFiles
      cs = V.fromList $ concatMap _scfe_packages scFiles
      rs = V.fromList $ concatMap _scfe_relations scFiles
      in do
      addRoots (V.map getIdentifier files)
      addFiles files
      addComponents cs
      addRelations rs
    Left err     -> stderrLog err
