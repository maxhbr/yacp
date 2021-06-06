{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.HHC.HHCUtils
  ( clusterifyHHC 
  , unDot
  , dropDir
  , computeMergedHHC
  , mergifyEA
  , spdxToHHC, parseSpdxToHHC
  ) where

import YACP.Core
import YACP.HHC.HHC
import YACP.ORT.ORT
import YACP.SPDX.SPDX

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Maybe as Maybe
import qualified Data.HashMap.Strict as HM
import qualified Control.Monad.State as MTL
import qualified System.FilePath as FP
import qualified System.Process as P
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.BFS as G
import qualified Data.Graph.Inductive.PatriciaTree as UG
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.UUID (UUID)
import System.Random (randomIO)

mergifyCopyright :: Maybe Text -> Maybe Text -> Maybe Text
mergifyCopyright left Nothing = left
mergifyCopyright Nothing right = right
mergifyCopyright (Just c) (Just c') = Just . T.unlines . List.nub $ (T.lines c) ++ (T.lines c')

cleanupLicense :: Maybe Text -> Maybe Text
cleanupLicense Nothing  = Nothing
cleanupLicense (Just "") = Nothing
cleanupLicense (Just t) = case T.stripPrefix ", " t of
    Nothing -> Just t
    t'      -> t'

mergifyEA :: HHC_ExternalAttribution -> HHC_ExternalAttribution -> Maybe HHC_ExternalAttribution
mergifyEA
  left@(HHC_ExternalAttribution
  { _source = HHC_ExternalAttribution_Source source _
  , _attributionConfidence = attributionConfidence
  , _comment = comment
  , _originId = originId
  , _identifier = identifier
  , _copyright = copyright
  , _licenseName = licenseName
  , _licenseText = licenseText
  , _preselected = preselected
  }) 
  (HHC_ExternalAttribution
  { _source = HHC_ExternalAttribution_Source source' _
  , _attributionConfidence = attributionConfidence'
  , _comment = comment'
  , _originId = originId'
  , _identifier = identifier'
  , _copyright = copyright'
  , _licenseName = licenseName'
  , _licenseText = licenseText'
  , _preselected = preselected'
  }) 
 = if (and [ source == source'
           , identifier `matchesIdentifier` identifier'
           , (cleanupLicense licenseName) == (cleanupLicense licenseName')])
   then Just (left{ _attributionConfidence = (attributionConfidence `min` attributionConfidence')
                  , _copyright = mergifyCopyright copyright copyright'
                  , _licenseName = cleanupLicense licenseName
                  , _licenseText = case licenseText of
                    Just lt -> Just lt
                    _       -> licenseText'
                  , _preselected = preselected || preselected'
                  })
   else Nothing

clusterifyEAMap :: Map.Map UUID HHC_ExternalAttribution -> [(UUID, HHC_ExternalAttribution, [UUID])]
clusterifyEAMap = let
        clusterifyEAMap' :: [(UUID, HHC_ExternalAttribution)]
                         -> [(UUID, HHC_ExternalAttribution, [UUID])]
                         -> [(UUID, HHC_ExternalAttribution, [UUID])]
        clusterifyEAMap' [] out               = out
        clusterifyEAMap' (i:ins) out = let
            clusterifyEAMap'' :: (UUID, HHC_ExternalAttribution)
                             -> [(UUID, HHC_ExternalAttribution, [UUID])]
                             -> [(UUID, HHC_ExternalAttribution, [UUID])]
            clusterifyEAMap'' (uuid, ea) []                              = [(uuid, ea, [])]
            clusterifyEAMap'' (uuid, ea) (ins@(in'@(uuid', ea', uuids):ins')) 
                | uuid == uuid' = ins
                | otherwise     = case mergifyEA ea ea' of
                    Just mergedEA -> (uuid', mergedEA, uuid : uuids):ins'
                    Nothing -> in' : (clusterifyEAMap'' (uuid, ea) ins')
            in clusterifyEAMap' ins (clusterifyEAMap'' i out)
      
    in (`clusterifyEAMap'` []) . Map.assocs

clusterifyHHC :: HHC -> HHC
clusterifyHHC (hhc@HHC { _externalAttributions = eas , _resourcesToAttributions = rtas }) = let
    clusters = clusterifyEAMap eas
    newEas = (Map.fromList . map (\(uuid, ea, _) -> (uuid, ea))) clusters
    lookupMap = ( Map.fromList . concatMap (\(uuid, _, uuids) -> map (\uuid' -> (uuid', uuid)) uuids)) clusters
    newRtas = (Map.map (\uuids -> List.nub (map (\uuid -> Map.findWithDefault uuid uuid lookupMap) uuids))) rtas
  in hhc { _externalAttributions = newEas
         , _resourcesToAttributions = newRtas
         }

unDot :: HHC -> HHC
unDot (hhc@HHC { _resources = rs , _resourcesToAttributions = rtas }) = let
  undotResources (rs@HHC_Resources { _dirs = dirs }) = let
    contentOfDot = Map.findWithDefault mempty "." dirs
    dirsWithoutDot = Map.delete "." dirs
    in rs {_dirs = dirsWithoutDot} <> contentOfDot
  undotRTAS = Map.mapKeys (\k -> case List.stripPrefix "/." k of
    Nothing -> k
    Just k' -> k')
  in hhc{ _resources = undotResources rs , _resourcesToAttributions = undotRTAS rtas }

dropDir :: FilePath -> HHC -> HHC
dropDir directoryName (hhc@HHC{ _resources = rs, _resourcesToAttributions = rtas}) = let
  filterResources (rs@HHC_Resources { _dirs = dirs }) = let
    filteredDirs = Map.filterWithKey (\key -> const (not (directoryName == key))) dirs
    filteredAndCleanedDirs = Map.map filterResources filteredDirs
    in rs{ _dirs = filteredAndCleanedDirs }
  filterRTAS = Map.filterWithKey (\key -> const (not (directoryName `List.isSubsequenceOf` key))) 
  in hhc{ _resources = filterResources rs, _resourcesToAttributions = filterRTAS rtas}

computeMergedHHC :: [FilePath] -> IO B.ByteString
computeMergedHHC inputPaths = let
    parseHHC :: FP.FilePath -> IO HHC
    parseHHC fp = do
      hPutStrLn stderr ("parse: " ++ fp)
      bs <- B.readFile fp
      case A.eitherDecode' bs of
        Right hhc -> return hhc
        Left err  -> do
            fail err
  in do
    hhcs <- mapM parseHHC inputPaths
    let finalHHC = clusterifyHHC $ mconcat (map unDot hhcs)
    return (A.encodePretty finalHHC)

spdxToHHC :: SPDXDocument -> IO HHC
spdxToHHC = let
    spdxFileToEA :: SPDXFile -> HHC_ExternalAttribution
    spdxFileToEA (SPDXFile 
      { _SPDXFile_SPDXID = spdxid
      , _SPDXFile_raw = raw
      , _SPDXFile_fileName = filename
      , _SPDXFile_fileTypes = _
      , _SPDXFile_checksums = _
      , _SPDXFile_LicenseConcluded = license
      , _SPDXFile_licenseInfoInFiles = _
      , _SPDXFile_licenseInfoFromFiles = _
      , _SPDXFile_licenseComments = _
      , _SPDXFile_copyrightText = copyright
      , _SPDXFile_comment = _
      , _SPDXFile_noticeText = notice
      , _SPDXFile_fileContributors = _
      , _SPDXFile_attributionTexts = attribution
      , _SPDXFile_fileDependencies = dependencies
      , _SPDXFile_name = name
      }) = HHC_ExternalAttribution
            { _source = HHC_ExternalAttribution_Source "SPDXFile" 100
            , _attributionConfidence = 100
            , _comment = (Just . T.pack . C8.unpack . A.encodePretty) raw
            , _originId = Nothing
            , _identifier = mempty
            , _copyright = Just $ T.pack copyright 
            , _licenseName = Just $ T.pack $ show license -- TODO
            , _licenseText = Nothing -- TODO
            , _preselected = False
            }

    spdxPackageToEA :: SPDXPackage -> HHC_ExternalAttribution
    spdxPackageToEA (SPDXPackage 
      { _SPDXPackage_SPDXID = spdxid
      , _SPDXPackage_raw = raw
      , _SPDXPackage_name = name
      , _SPDXPackage_versionInfo = version
      , _SPDXPackage_packageFileName = _
      , _SPDXPackage_supplier = _
      , _SPDXPackage_originator = _
      , _SPDXPackage_downloadLocation = _
      , _SPDXPackage_filesAnalyzed = _
      , _SPDXPackage_packageVerificationCode = _
      , _SPDXPackage_checksums = _
      , _SPDXPackage_homepage = _
      , _SPDXPackage_sourceInfo = _
      , _SPDXPackage_licenseConcluded = _
      , _SPDXPackage_licenseInfoFromFiles = _
      , _SPDXPackage_licenseDeclared = license
      , _SPDXPackage_licenseComments = _
      , _SPDXPackage_copyrightText = copyright
      , _SPDXPackage_summary = _
      , _SPDXPackage_description = _
      , _SPDXPackage_comment = _
      , _SPDXPackage_attributionTexts = _
      , _SPDXPackage_hasFiles = _
      }) = HHC_ExternalAttribution
          { _source = HHC_ExternalAttribution_Source "SPDXPackage" 100
          , _attributionConfidence = 100
          , _comment = (Just . T.pack . C8.unpack . A.encodePretty) raw
          , _originId = Nothing
          , _identifier = PURL (Just "pkg") Nothing Nothing name version Nothing Nothing
          , _copyright = case copyright of
            SPDXJust copyright' -> (Just . T.pack)  copyright'
            _                   -> Nothing
          , _licenseName = Just $ T.pack $ show license -- TODO
          , _licenseText = Nothing -- TODO
          , _preselected = True
          } 

    spdxFileOrPackageToEA :: Either SPDXFile SPDXPackage -> HHC_ExternalAttribution
    spdxFileOrPackageToEA (Left f) = spdxFileToEA f
    spdxFileOrPackageToEA (Right p) = spdxPackageToEA p
    in \(spdx@SPDXDocument
      { _SPDX_comment = _
      , _SPDX_creationInfo = _
      , _SPDX_name = name
      , _SPDX_documentDescribes = roots
      , _SPDX_files = files
      , _SPDX_packages = packages
      , _SPDX_relationships = relationships
      }) -> let
        hhcMetadata = mempty { _metadata = Just (HHC_Metadata name "") }
        (graph, idsToIdxs, indxsToIds) = spdxDocumentToGraph spdx
        trees = map (\root -> let
          indx = Map.findWithDefault (undefined) root idsToIdxs
          in G.lbft indx graph :: [G.LPath SPDXRelationship])
          roots
        inits = let
          addS :: [G.LNode SPDXRelationship] -> Maybe ([G.LNode SPDXRelationship],  Either SPDXFile SPDXPackage)
          addS [] = Nothing
          addS path = let
              (node,_) = last path
            in case G.lab graph node of 
              Just s -> Just (path, s)
              _ -> Nothing
          in concatMap (Maybe.catMaybes . map addS . List.nub . concatMap List.inits . map (reverse . G.unLPath)) trees
        initToHHC :: ([G.LNode SPDXRelationship],  Either SPDXFile SPDXPackage) -> IO HHC
        initToHHC (p, s) = let
          ea = spdxFileOrPackageToEA s
          rs = ('/':) . joinPath $ map ((\k -> case G.lab graph k of
            Just (Left (SPDXFile {_SPDXFile_SPDXID = spdxid, _SPDXFile_name = name})) -> case name of
              Just name'  -> name'
              _ -> spdxid
            Just (Right (SPDXPackage {_SPDXPackage_SPDXID = spdxid, _SPDXPackage_name = name})) -> case name of
              "" -> spdxid
              _  -> name
            Nothing -> "??") . fst) p
          rsFull = case s of 
            Left (SPDXFile{_SPDXFile_fileName = fn}) -> rs </> fn
            _      -> rs ++ "/"
          in do
            uuid <- randomIO
            return $ mempty
              { _resources = fpToResources (case s of 
                Left _ -> FileType_File
                _      -> FileType_Folder) rsFull
              , _externalAttributions = Map.singleton uuid ea
              , _resourcesToAttributions = Map.singleton rsFull [uuid]
              }
      in do
        pieces <- mapM initToHHC inits
        return $ mconcat (hhcMetadata : pieces)

parseSpdxToHHC :: FilePath -> IO B.ByteString
parseSpdxToHHC inputPath = do
  spdx <- parseSPDXDocument inputPath
  hhc <- spdxToHHC spdx
  return (A.encodePretty hhc)