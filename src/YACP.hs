{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module YACP
  ( module X
  , parseScancodeFile
  , parseSPDXFile
  , parseCycloneDXFile
  ) where

import YACP.Core as X
import YACP.OrtCollector as X
import YACP.ScancodeCollector as X
import YACP.SPDXCollector as X
import YACP.CycloneDXCollector as X
import YACP.ComputeGraph as X
import YACP.PPState as X
import YACP.Plantuml as X
import YACP.Graphviz as X

import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.State as MTL

parseBSFromFile :: (B.ByteString -> YACP a) -> FilePath -> YACP a
parseBSFromFile fun path = do
  bs <- MTL.liftIO $ B.readFile path
  fun bs

parseScancodeFile :: FilePath -> YACP ()
parseScancodeFile = parseBSFromFile parseScancodeBS

parseSPDXFile :: FilePath -> YACP ()
parseSPDXFile = parseBSFromFile parseSPDXBS

parseCycloneDXFile :: FilePath -> YACP ()
parseCycloneDXFile = parseBSFromFile parseCycloneDXBS
