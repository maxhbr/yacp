{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.HHC.HHCUtils
  ( clusterifyHHC 
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
import qualified System.Process as P
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.ByteString.Lazy as B
import Data.UUID (UUID)
import System.Random (randomIO)

mergyfyCopyright :: Maybe Text -> Maybe Text -> Maybe Text
mergyfyCopyright left Nothing = left
mergyfyCopyright Nothing right = right
mergyfyCopyright (Just c) (Just c') = Just . T.unlines . List.nub $ (T.lines c) ++ (T.lines c')

cleanupLicense :: Maybe Text -> Maybe Text
cleanupLicense Nothing  = Nothing
cleanupLicense (Just "") = Nothing
cleanupLicense (Just t) = case T.stripPrefix ", " t of
    Nothing -> Just t
    t'      -> t'

mergyfyEA :: HHC_ExternalAttribution -> HHC_ExternalAttribution -> Maybe HHC_ExternalAttribution
mergyfyEA
  left@(HHC_ExternalAttribution
  { _source = HHC_ExternalAttribution_Source source _
  , _attributionConfidence = attributionConfidence
  , _comment = comment
  , _originId = originId
  , _identifier = identifier
  , _copyright = copyright
  , _licenseName = licenseName
  }) 
  (HHC_ExternalAttribution
  { _source = HHC_ExternalAttribution_Source source' _
  , _attributionConfidence = attributionConfidence'
  , _comment = comment'
  , _originId = originId'
  , _identifier = identifier'
  , _copyright = copyright'
  , _licenseName = licenseName'
  }) 
 = if (and [ source == source'
           , identifier `matchesIdentifier` identifier'
           , (cleanupLicense licenseName) == (cleanupLicense licenseName')])
   then Just (left{ _attributionConfidence = (attributionConfidence `min` attributionConfidence')
                  , _copyright = mergyfyCopyright copyright copyright'
                  , _licenseName = cleanupLicense licenseName
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
                | otherwise     = case mergyfyEA ea ea' of
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