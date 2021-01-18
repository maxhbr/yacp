{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.OrtCollector
  (OrtFile (..), OrtResult (..)
  , parseOrtFile, parseOrtBS
  ) where

import YACP.Core
import YACP.ParserHelper

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List (intercalate)
import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Vector as V
import qualified System.FilePath as FP
import qualified Control.Monad.State as MTL
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec as SPDX

{-
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
 -}
parseVcs :: A.Object -> A.Parser (Maybe Identifier)
parseVcs v = let
  parseVcsGH vcsUrl vcsRevision vcsPath =
      if "https://github.com/" `List.isPrefixOf` vcsUrl
        then Just $ let
        path = case List.stripPrefix "https://github.com/" vcsUrl of
          Just postfix -> postfix
          Nothing -> vcsUrl
        namespace = FP.takeDirectory path
        name = FP.takeBaseName path
        in PURL
           (Just "pkg")
           (Just "github")
           (Just namespace)
           name
           (Just vcsRevision)
           Nothing
           (Just vcsPath)
        else Nothing
  in do
  vcsProcessed <- v A..:? "vcs_processed"
  vcsRaw <- v A..:? "vcs"
  let vcs = case vcsProcessed of
        Just vcs -> Just vcs
        Nothing -> case vcsRaw of
          Just vcs -> Just vcs
          Nothing -> Nothing
  case vcs of
    Just vcs' -> do
      vcsType <- vcs' A..: "type" :: A.Parser String
      vcsUrl <- vcs' A..: "url" :: A.Parser String
      vcsRevision <- vcs' A..: "revision" :: A.Parser String
      vcsPath <- vcs' A..: "path" :: A.Parser String
      case parseVcsGH vcsUrl vcsRevision vcsPath of
        Just i -> return (Just i)
        Nothing -> if vcsUrl /= "" && vcsRevision /= ""
          then return (Just (Identifier (vcsType ++ "+" ++ vcsUrl ++ "@" ++ vcsRevision ++ "#" ++ vcsPath)))
          else return Nothing
    Nothing -> return Nothing

{-
    - package:
        id: "PyPI::babel:2.9.0"
        purl: "pkg:pypi/babel@2.9.0"
        declared_licenses:
        - "BSD"
        - "BSD License"
        declared_licenses_processed:
          spdx_expression: "BSD-3-Clause"
          mapped:
            BSD: "BSD-3-Clause"
            BSD License: "BSD-3-Clause"
        description: "Internationalization utilities"
        homepage_url: "http://babel.pocoo.org/"
        binary_artifact:
          url: "https://files.pythonhosted.org/packages/dd/a5/81076e10b5ef74493cf08a8e419e61b64324c9c55db4aa7f89c0240c4873/Babel-2.9.0-py2.py3-none-any.whl"
          hash:
            value: "f4ed917cb45bcc760a4f652890bdffa8"
            algorithm: "MD5"
        source_artifact:
          url: "https://files.pythonhosted.org/packages/41/1b/5ed6e564b9ca54318df20ebe5d642ab25da4118df3c178247b8c4b26fa13/Babel-2.9.0.tar.gz"
          hash:
            value: "bfc803874aa71e9e9bd54bdd1ce944ba"
            algorithm: "MD5"
        vcs:
          type: ""
          url: ""
          revision: ""
          path: ""
        vcs_processed:
          type: ""
          url: ""
          revision: ""
          path: ""
-}
packageJsonToComponent :: A.Object -> A.Parser Component
packageJsonToComponent v = let
    packageParser = v A..: "package"
    idParser = fmap Identifier (packageParser >>= (A..: "id"))
    purlParser = fmap parsePURL (packageParser >>= (A..: "purl"))
    parseArtifact :: A.Object -> A.Parser Identifier
    parseArtifact v = do
      urlId <- fmap (maybeToList . fmap UrlIdentifier) (v A..:? "url") :: A.Parser [Identifier]
      hash <- v A..:? "hash"
      hashId <- case hash of
        Just hash' -> do
          hashValue <- hash' A..: "value" :: A.Parser String
          hashAlgorithm <- hash' A..:? "algorithm" :: A.Parser (Maybe String)
          return [Hash hashAlgorithm hashValue]
        _ -> return []
      return (mconcat (urlId ++ hashId))
  in do
  id1 <- idParser
  id2 <- purlParser

  relFromVcs <- fmap (map (\vcs -> Relation vcs CONTAINS id1) . maybeToList) (packageParser >>= parseVcs)
  relFromBinaryArtifact <- do
    artifact <- packageParser >>= (A..:? "binary_artifact") :: A.Parser (Maybe A.Object)
    case artifact of
      Just artifact' -> do
        artId <- parseArtifact artifact'
        return [Relation id1 GENERATES artId]
      Nothing -> return []
  relFromSourceArtifact <- do
    artifact <- packageParser >>= (A..:? "source_artifact") :: A.Parser (Maybe A.Object)
    case artifact of
      Just artifact' -> do
        artId <- parseArtifact artifact'
        return [Relation id1 GENERATED_FROM artId]
      Nothing -> return []
  let rels = relFromVcs ++ relFromBinaryArtifact ++ relFromSourceArtifact
  Component
     <$> pure (id1 <> id2)
     <*> fmap parseLicenses (packageParser >>= (A..: "declared_licenses"))
     <*> pure (V.singleton (A.Object v))
     <*> pure rels
     <*> pure []

projectJsonToComponentWithRelations :: A.Object -> A.Parser Component
projectJsonToComponentWithRelations v = let
    idParser = fmap Identifier (v A..: "id")

    componentParser cId rels = do
      Component
        <$> pure cId
        <*> fmap parseLicenses (v A..: "declared_licenses")
        <*> pure (V.singleton (A.Object v))
        <*> pure rels
        <*> pure []
    definedInParser cId = fmap ((\fp -> Relation fp METAFILE_OF cId) . PathIdentifier) (v A..: "definition_file_path")
  in do
  cId <- idParser
  definedIn <- definedInParser cId
  scopes <- (v A..: "scopes") >>= scopesJsonToRelations cId
  relFromVcs <- fmap (map (\vcs -> Relation vcs CONTAINS cId) . maybeToList) (parseVcs v)
  let rels = definedIn : scopes ++ relFromVcs
  componentParser cId rels

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
scopesJsonToRelations target v = let
  scopesJsonToRelations' :: A.Value -> A.Parser [Relation]
  scopesJsonToRelations' = A.withObject "" $ \v' -> do

    scopeName <- v' A..:? "name" :: A.Parser (Maybe String)
    let relationType = case scopeName of
          Just "build" -> BUILD_DEPENDENCY_OF
          Just "dev" -> DEV_DEPENDENCY_OF
          Just "optional" -> OPTIONAL_DEPENDENCY_OF
          Just "provided" -> PROVIDED_DEPENDENCY_OF
          Just "test" -> TEST_DEPENDENCY_OF
          Just "runtime" -> RUNTIME_DEPENDENCY_OF
          _ -> DEPENDENCY_OF

    dependencies <- v' A..:? "dependencies" :: A.Parser (Maybe A.Array)
    case dependencies of
      Just dependencies' -> do
        deps1 <- fmap (map (\src -> Relation src relationType target)
                        . map Identifier)
                (mapM (A.withObject "" (A..: "id")) (V.toList dependencies'))

        target' <- fmap ((target `fromMaybe`) . (fmap Identifier)) (v' A..:? "id")
        otherDeps <- scopesJsonToRelations target' dependencies'
        return (deps1 ++ otherDeps)
      Nothing -> return []
  in do
  fmap concat $ mapM scopesJsonToRelations' v

data OrtResult
  = OrtResult
  { _or_start_time :: String
  , _or_end_time :: String
  , _or_projects :: Vector Component
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
  { _of_Root :: Maybe Identifier
  , _of_Analyzer :: OrtResult
  -- , _of_Scanner :: Maybe OrtResult
  } deriving (Show)
instance A.FromJSON OrtFile where
  parseJSON = A.withObject "OrtFile" $ \v -> do
    r <- v A..: "repository"
    OrtFile
        <$> parseVcs r
        <*> v A..: "analyzer"
        -- <*> v A..:? "scanner"

parseOrtFile :: FilePath -> YACP ()
parseOrtFile path = do
  bs <- MTL.liftIO $ B.readFile path
  parseOrtBS bs
parseOrtBS :: B.ByteString -> YACP ()
parseOrtBS bs =
  case (A.eitherDecode bs :: Either String OrtFile) of
    Right (OrtFile{_of_Root = root, _of_Analyzer = analyzerResult}) -> case analyzerResult of
      OrtResult{_or_projects = ps, _or_packages = cs} -> do
        addComponents cs
        addComponents ps
        let projectIds = V.map getIdentifier ps
        case root of
          Just root' -> addRelations $ V.map (\pId -> Relation root' CONTAINS pId) projectIds
          Nothing -> return ()
        addRoots projectIds
    Left err     -> stderrLog err
