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
  ) where

import YACP.Core
import YACP.HHC.HHC

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
import qualified Data.ByteString.Lazy as B
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
  , _preselected = preselected'
  }) 
 = if (and [ source == source'
           , identifier `matchesIdentifier` identifier'
           , (cleanupLicense licenseName) == (cleanupLicense licenseName')])
   then Just (left{ _attributionConfidence = (attributionConfidence `min` attributionConfidence')
                  , _copyright = mergifyCopyright copyright copyright'
                  , _licenseName = cleanupLicense licenseName
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
      case (A.eitherDecode' bs) of
        Right hhc -> return hhc
        Left err  -> do
            hPutStrLn stderr err
            undefined
  in do
    hhcs <- mapM parseHHC inputPaths
    let finalHHC = clusterifyHHC $ mconcat (map unDot hhcs)
    return (A.encodePretty finalHHC)