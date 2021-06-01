{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module YACP.HHCCollector
  ( parseHHCBS
  ) where

import YACP.Core
import YACP.HHC.HHC
import YACP.HHC.HHCUtils
import YACP.ParserHelper

import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.List (intercalate)
import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified System.FilePath as FP
import qualified Control.Monad.State as MTL
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec as SPDX

addEAToPath :: FilePath -> Maybe HHC_ExternalAttribution -> YACP ()
addEAToPath _ Nothing    = pure ()
addEAToPath fp (Just ea@(HHC_ExternalAttribution{_identifier=identifier,_licenseName=licenseName})) = let
    file = mkFile fp
  in do
    addFile file
    let component = Component identifier (fmap (parseLicense . T.unpack) licenseName) (V.singleton (A.toJSON ea)) [] []
    componentIdentifier <- addComponent component
    let relation = Relation (getIdentifier file) GENERATES componentIdentifier
    addRelation relation
    pure ()

parseHHCBS :: B.ByteString -> YACP ()
parseHHCBS bs =
  case (A.eitherDecode bs :: Either String HHC) of
    Right result -> do
      MTL.liftIO $ writeHHCStats result
      let (merged@HHC{_resourcesToAttributions=rtas, _externalAttributions=eas}) = clusterifyHHC result
      MTL.liftIO $ writeHHCStats merged
      mapM_ (\(path, vals) -> mapM_ (\val -> addEAToPath path (val `Map.lookup` eas)) vals)
            (Map.toList rtas)
    Left err -> stderrLog err