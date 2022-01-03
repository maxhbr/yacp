{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeFamilies              #-}

module YACP.Model.Licenseable
  ( Licenseable(..)
  , SPDX.renderSpdxLicense
  ) where

import           YACP.Model.Identifier
import           YACP.Model.Relation

import qualified PURL.PURL               as PURL

import qualified Control.Monad.State     as MTL
import qualified Data.Aeson              as A
import qualified Data.Aeson.Types        as A
import           Data.List               (nub)
import           Data.List.Split         (splitOn)
import           Data.Maybe              (fromMaybe, maybeToList)
import qualified Data.Monoid             (mconcat)
import           Data.String             (IsString (..))
import qualified Data.Text               as T
import           Data.UUID               (UUID)
import qualified Data.Vector             as V
import qualified Distribution.Parsec     as SPDX
import qualified Distribution.SPDX       as SPDX
import qualified Distribution.SPDX.Extra as SPDX
import qualified SPDX.Document           as SPDX
import           System.Console.Pretty   (Color (Green), color)
import qualified System.FilePath         as FP
import           System.IO               (hPutStrLn, stderr)
import           System.Random           (randomIO)

class Licenseable a where
  getLicense :: a -> SPDX.MaybeLicenseExpression
  getRawLicense :: a -> SPDX.SPDXMaybe SPDX.LicenseExpression
  getRawLicense = SPDX.unMLicExp . getLicense
  showLicense :: a -> String
  showLicense = show . getLicense
