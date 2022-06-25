{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.ComponentDetectionReader
  ( readComponentDetectionFile
  , readComponentDetectionBS
  -- for testing:
  , parseComponentDetectionBS
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

newtype DependencyGraph
  = DependencyGraph (Map.Map String DependencyGraph)
  deriving (Eq, Show)
instance A.FromJSON DependencyGraph where
  parseJSON =
    let keyMapToDependencyGraph :: A.KeyMap A.Value -> DependencyGraph
        keyMapToDependencyGraph v = DependencyGraph $ if A.null v
          then mempty
          else
            ( Map.map
                  (keyMapToDependencyGraph . \case
                    A.Object v' -> v'
                    _           -> mempty
                  )
              . Map.mapKeys show
              . A.toMap
              )
              v
    in  \(A.Object v) -> pure $ keyMapToDependencyGraph v

data FoundComponent = FoundComponent
  { _locationsFoundAt        :: [FilePath]
  , _id                      :: String
  , _packageURL              :: PURL
  , _isDevelopmentDependency :: Maybe Bool
  , _dependencyScope         :: Maybe String
  -- TODO: topLevelReferrers?
  }
  deriving (Eq, Show)
instance A.FromJSON FoundComponent where
  parseJSON (A.Object v) = do
    component <- v A..: "component"
    purl      <- component A..: "packageUrl"
    FoundComponent
      <$>   v
      A..:  "locationsFoundAt"
      <*>   component
      A..:  "id"
      <*>   (     PURL
            <$>   purl
            A..:? "Scheme"
            <*>   purl
            A..:? "Type"
            <*>   purl
            A..:? "Namespace"
            <*>   purl
            A..:  "Name"
            <*>   purl
            A..:? "Version"
            <*>   purl
            A..:? "Qualifiers"
            <*>   purl
            A..:? "Subpath"
            )
      <*>   v
      A..:? "isDevelopmentDependency"
      <*>   v
      A..:? "dependencyScope"

data ComponentDetection = ComponentDetection
  { _dependencyGraphs :: Map.Map FilePath DependencyGraph
  , _componentsFound  :: [FoundComponent]
  }
  deriving (Eq, Show)

instance A.FromJSON ComponentDetection where
  parseJSON (A.Object v) =
    ComponentDetection
      <$>  v
      A..: "dependencyGraphs"
      <*>  v
      A..: "componentsFound"

parseComponentDetectionBS
  :: B.ByteString -> Either YACPIssue ComponentDetection
parseComponentDetectionBS bs = case A.eitherDecode bs of
  Right cd  -> Right cd
  Left  err -> Left (YACPParsingIssue err)

applyComponentDetection :: Origin -> ComponentDetection -> YACP ()
applyComponentDetection o (ComponentDetection dgs cfs) =
  let
    applyDependencyGraphs :: Map.Map FilePath DependencyGraph -> YACP ()
    applyDependencyGraphs = let in \_ -> pure ()
-- idfyGraph :: Map.Map FilePath DependencyGraph -> 
    applyFoundComponent :: FoundComponent -> YACP ()
    applyFoundComponent FoundComponent { _locationsFoundAt = locationsFoundAt, _id = componentId, _packageURL = packageUrl }
      = let identifier = PurlIdentifier packageUrl <> flexibilizePURL packageUrl <> Identifier componentId
            statements = setOrigin o . (Statements . V.fromList) $ map
              (Statement identifier)
              (map (FoundManifestFile . AbsolutePathIdentifier) locationsFoundAt
              )
        in  addStatements statements
  in
    do
      applyDependencyGraphs dgs
      mapM_ applyFoundComponent cfs

readComponentDetectionBS :: Origin -> B.ByteString -> YACP (Maybe YACPIssue)
readComponentDetectionBS o bs = case parseComponentDetectionBS bs of
  Right cd -> do
    applyComponentDetection o cd
    return (Nothing)
  Left issue -> return (Just issue)

readComponentDetectionFile :: FilePath -> YACP ()
readComponentDetectionFile f = readBSFromFile (readComponentDetectionBS (OriginToolReport "component-detection" f)) f
