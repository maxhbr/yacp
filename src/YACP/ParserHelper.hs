{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.ParserHelper where

import YACP.Core

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

parseLicense :: String -> SPDX.LicenseExpression
parseLicense str = (`SPDX.ELicense` Nothing) $ case SPDX.eitherParsec str of
  Right lic -> SPDX.ELicenseId lic
  _         -> SPDX.ELicenseRef $ SPDX.mkLicenseRef' Nothing str

parseLicenses :: [String] -> Maybe SPDX.LicenseExpression
parseLicenses [] = Nothing
parseLicenses ls = let

  parseLicenses' :: [String] -> SPDX.LicenseExpression
  parseLicenses' [l] = parseLicense l
  parseLicenses' (l:ls) = parseLicense l `SPDX.EAnd` (parseLicenses' ls)
  in Just (parseLicenses' ls)
