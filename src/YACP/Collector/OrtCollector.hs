{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.OrtCollector
  (OrtFile (..), OrtResult (..)
  , parseOrtBS
  ) where

import YACP.Core
import YACP.ParserHelper
import YACP.ORT.ORT

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

parseOrtBS :: B.ByteString -> YACP (Maybe YACPIssue)
parseOrtBS bs =
  case (A.eitherDecode bs :: Either String OrtFile) of
    Right (OrtFile{_of_Root = root, _of_Analyzer = analyzerResult, _of_Scanner = scannerResult}) -> case analyzerResult `fromMaybe` scannerResult of
      OrtResult{_or_projects = ps, _or_packages = cs} -> do
        addComponents cs
        addComponents ps
        let projectIds = V.map getIdentifier ps
        case root of
          Just root' -> addRelations $ V.map (\pId -> Relation root' CONTAINS pId) projectIds
          Nothing -> return ()
        addRoots projectIds
        return Nothing
    Left err     -> do
      stderrLog err
      return (Just (YACPParsingIssue err))
