{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Collectors.OrtCollector
  (OrtFile (..), OrtResult (..)
  , parseOrtFile, parseOrtBS
  ) where

import YACP.MyPrelude
import YACP.Model

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Control.Monad.State as MTL
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec as SPDX

parseLicenses :: [String] -> Maybe SPDX.LicenseExpression
parseLicenses [] = Nothing
parseLicenses ls = let
  parseLicense :: String -> SPDX.LicenseExpression
  parseLicense str = (`SPDX.ELicense` Nothing) $ case SPDX.eitherParsec str of
    Right lic -> SPDX.ELicenseId lic
    _         -> SPDX.ELicenseRef $ SPDX.mkLicenseRef' Nothing str

  parseLicenses' :: [String] -> SPDX.LicenseExpression
  parseLicenses' [l] = parseLicense l
  parseLicenses' (l:ls) = parseLicense l `SPDX.EAnd` (parseLicenses' ls)
  in Just (parseLicenses' ls)

{-
  {
    "package" : {
      "id" : "PyPI::Jinja2:2.11.2",
      "purl" : "pkg:pypi/Jinja2@2.11.2",
      "declared_licenses" : [ "BSD License", "BSD-3-Clause" ],
      "declared_licenses_processed" : {
        "spdx_expression" : "BSD-3-Clause",
        "mapped" : {
          "BSD License" : "BSD-3-Clause"
        }
      },
      "description" : "A very fast and expressive template engine.",
      "homepage_url" : "https://palletsprojects.com/p/jinja/",
      "binary_artifact" : {
        "url" : "https://files.pythonhosted.org/packages/30/9e/f663a2aa66a09d838042ae1a2c5659828bb9b41ea3a6efa20a20fd92b121/Jinja2-2.11.2-py2.py3-none-any.whl",
        "hash" : {
          "value" : "e36888c21cb0f6716b9987be2972744d",
          "algorithm" : "MD5"
        }
      },
      "source_artifact" : {
        "url" : "https://files.pythonhosted.org/packages/64/a7/45e11eebf2f15bf987c3bc11d37dcc838d9dc81250e67e4c5968f6008b6c/Jinja2-2.11.2.tar.gz",
        "hash" : {
          "value" : "0362203b22547abca06ed1082bc1e7b4",
          "algorithm" : "MD5"
        }
      },
      "vcs" : {
        "type" : "",
        "url" : "",
        "revision" : "",
        "path" : ""
      },
      "vcs_processed" : {
        "type" : "",
        "url" : "",
        "revision" : "",
        "path" : ""
      }
    },
    "curations" : [ ]
  }
-}

packageJsonToComponent :: A.Object -> A.Parser Component
packageJsonToComponent v = let
    packageParser = v A..: "package"
    idParser = fmap Identifier (packageParser >>= (A..: "id"))
    purlParser = fmap parsePURL (packageParser >>= (A..: "purl"))
  in do
  id1 <- idParser
  id2 <- purlParser
  Component
     <$> pure (id1 <> id2)
     <*> fmap parseLicenses (packageParser >>= (A..: "declared_licenses"))
     <*> pure (V.singleton (A.Object v))

{-
    - id: "PIP::black-requirementstest_requirements:de510478d9b12490eddf93a785494f726897363b"
      definition_file_path: "test_requirements.txt"
      declared_licenses:
      - "MIT"
      - "MIT License"
      declared_licenses_processed:
        spdx_expression: "MIT"
        mapped:
          MIT License: "MIT"
      vcs:
        type: ""
        url: ""
        revision: ""
        path: ""
      vcs_processed:
        type: "Git"
        url: "https://github.com/psf/black.git"
        revision: "de510478d9b12490eddf93a785494f726897363b"
        path: ""
      homepage_url: "https://github.com/psf/black"
      scopes:
      - name: "install"
        dependencies:
        - id: "PyPI::coverage:5.3.1"
        - id: "PyPI::parameterized:0.8.1"
        - id: "PyPI::pre-commit:2.9.3"
          dependencies:
          - id: "PyPI::cfgv:3.2.0"
          - id: "PyPI::identify:1.5.12"
          - id: "PyPI::importlib-metadata:3.4.0"
            dependencies:
            - id: "PyPI::typing-extensions:3.7.4.3"
            - id: "PyPI::zipp:3.4.0"
          - id: "PyPI::importlib-resources:5.0.0"
            dependencies:
            - id: "PyPI::zipp:3.4.0"
          - id: "PyPI::nodeenv:1.5.0"
          - id: "PyPI::pyyaml:5.3.1"
          - id: "PyPI::toml:0.10.2"
          - id: "PyPI::virtualenv:20.3.1"
            dependencies:
            - id: "PyPI::appdirs:1.4.4"
            - id: "PyPI::distlib:0.3.1"
            - id: "PyPI::filelock:3.0.12"
            - id: "PyPI::importlib-metadata:3.4.0"
              dependencies:
              - id: "PyPI::typing-extensions:3.7.4.3"
              - id: "PyPI::zipp:3.4.0"
            - id: "PyPI::importlib-resources:5.0.0"
              dependencies:
              - id: "PyPI::zipp:3.4.0"
            - id: "PyPI::six:1.15.0"
        - id: "PyPI::pytest-cases:3.1.1"
          dependencies:
          - id: "PyPI::decopatch:1.4.8"
            dependencies:
            - id: "PyPI::makefun:1.9.5"
          - id: "PyPI::makefun:1.9.5"
        - id: "PyPI::pytest-mock:3.5.1"
          dependencies:
          - id: "PyPI::pytest:6.2.1"
            dependencies:
            - id: "PyPI::attrs:20.3.0"
            - id: "PyPI::importlib-metadata:3.4.0"
              dependencies:
              - id: "PyPI::typing-extensions:3.7.4.3"
              - id: "PyPI::zipp:3.4.0"
            - id: "PyPI::iniconfig:1.1.1"
            - id: "PyPI::packaging:20.8"
              dependencies:
              - id: "PyPI::pyparsing:2.4.7"
            - id: "PyPI::pluggy:0.13.1"
              dependencies:
              - id: "PyPI::importlib-metadata:3.4.0"
                dependencies:
                - id: "PyPI::typing-extensions:3.7.4.3"
                - id: "PyPI::zipp:3.4.0"
            - id: "PyPI::py:1.10.0"
            - id: "PyPI::toml:0.10.2"
        - id: "PyPI::tox:3.21.1"
          dependencies:
          - id: "PyPI::filelock:3.0.12"
          - id: "PyPI::importlib-metadata:3.4.0"
            dependencies:
            - id: "PyPI::typing-extensions:3.7.4.3"
            - id: "PyPI::zipp:3.4.0"
          - id: "PyPI::packaging:20.8"
            dependencies:
            - id: "PyPI::pyparsing:2.4.7"
          - id: "PyPI::pluggy:0.13.1"
            dependencies:
            - id: "PyPI::importlib-metadata:3.4.0"
              dependencies:
              - id: "PyPI::typing-extensions:3.7.4.3"
              - id: "PyPI::zipp:3.4.0"
          - id: "PyPI::py:1.10.0"
          - id: "PyPI::six:1.15.0"
          - id: "PyPI::toml:0.10.2"
          - id: "PyPI::virtualenv:20.3.1"
            dependencies:
            - id: "PyPI::appdirs:1.4.4"
            - id: "PyPI::distlib:0.3.1"
            - id: "PyPI::filelock:3.0.12"
            - id: "PyPI::importlib-metadata:3.4.0"
              dependencies:
              - id: "PyPI::typing-extensions:3.7.4.3"
              - id: "PyPI::zipp:3.4.0"
            - id: "PyPI::importlib-resources:5.0.0"
              dependencies:
              - id: "PyPI::zipp:3.4.0"
            - id: "PyPI::six:1.15.0"
-}
projectJsonToComponentWithRelations :: A.Object -> A.Parser (Component, [Relation])
projectJsonToComponentWithRelations v = let
    idParser = fmap Identifier (v A..: "id")
    componentParser cId = do
      Component
        <$> pure cId
        <*> fmap parseLicenses (v A..: "declared_licenses")
        <*> pure (V.singleton (A.Object v))
    definedInParser cId = fmap ((\fp -> Relation fp METAFILE_OF cId) . PathIdentifier) (v A..: "definition_file_path")
  in do
  cId <- idParser
  component <- componentParser cId
  definedIn <- definedInParser cId
  scopes <- (v A..: "scopes") >>= scopesJsonToRelations cId
  return (component, definedIn : scopes)

{-
  "scopes" : [ {
    "name" : "install",
    "dependencies" : [ {
      "id" : "PyPI::coverage:5.3.1"
    }, {
      "id" : "PyPI::parameterized:0.8.1"
    }, {
      "id" : "PyPI::pre-commit:2.9.3",
      "dependencies" : [ {
        "id" : "PyPI::cfgv:3.2.0"
      } ]
     } ]
   } ]
-}
scopesJsonToRelations :: Identifier -> A.Array -> A.Parser [Relation]
scopesJsonToRelations src v = let
  scopesJsonToRelations' :: A.Value -> A.Parser [Relation]
  scopesJsonToRelations' = A.withObject "" $ \v' -> do
    maybeId <- fmap ((src `fromMaybe`) . (fmap Identifier)) (v' A..:? "id")
    dependencies <- v' A..:? "dependencies" :: A.Parser (Maybe A.Array)
    case dependencies of
      Just dependencies' -> do
        deps1 <- fmap (map (Relation src DEPENDS_ON)
                        . map Identifier)
                (mapM (A.withObject "" (A..: "id")) (V.toList dependencies'))
        otherDeps <- scopesJsonToRelations maybeId dependencies'
        return (deps1 ++ otherDeps)
      Nothing -> return []
  in do
  fmap concat $ mapM scopesJsonToRelations' v

-- scopesJsonToRelations :: Identifier -> A.Array -> A.Parser [Relation]
-- scopesJsonToRelations cId v = let
--   parseDependencies :: A.Value -> A.Parser [Relation]
--   parseDependencies = A.withObject "" $ \v' -> do
--     maybeId <- fmap ((cId `fromMaybe`) . (fmap Identifier)) (v' A..:? "id")
--     deps <- v' A..: "dependencies"
--     undefined
--   in do
--   rs <- mapM parseDependencies v
--   return [] -- TODO

-- data OrtRepository
--   = OrtRepository
--   deriving (Generic, Eq, Show)
-- instance A.FromJSON OrtRepository where
--   parseJSON = A.withObject "OrtRepository" $ \v -> OrtRepository
data OrtResult
  = OrtResult
  { _or_start_time :: String
  , _or_end_time :: String
  , _or_projects :: Vector (Component, [Relation])
  , _or_packages :: Vector Component
  } deriving (Show)
instance A.FromJSON OrtResult where
  parseJSON = A.withObject "OrtResult" $ \v -> let
    resultParser = v A..: "result"
    in OrtResult
       <$> v A..: "start_time"
       <*> v A..: "end_time"
       <*> fmap V.fromList (resultParser >>= (A..: "projects") >>= (mapM projectJsonToComponentWithRelations))
       <*> fmap V.fromList (resultParser >>= (A..: "packages") >>= (mapM packageJsonToComponent))

data OrtFile
  = OrtFile
  { _of_Analyzer :: OrtResult
  -- , _of_Scanner :: Maybe OrtResult
  } deriving (Show)
instance A.FromJSON OrtFile where
  parseJSON = A.withObject "OrtFile" $ \v -> OrtFile
        <$> v A..: "analyzer"
        -- <*> v A..:? "scanner"

parseOrtFile :: FilePath -> YACP ()
parseOrtFile path = do
  bs <- MTL.liftIO $ B.readFile path
  parseOrtBS bs
parseOrtBS :: B.ByteString -> YACP ()
parseOrtBS bs = do
  case (A.eitherDecode bs :: Either String OrtFile) of
    Right (OrtFile{_of_Analyzer = analyzerResult}) -> case analyzerResult of
      (OrtResult{_or_projects = ps, _or_packages = cs}) -> do
         addComponents cs
         addComponentsWithRelations ps
    Left err     -> stderrLog err
