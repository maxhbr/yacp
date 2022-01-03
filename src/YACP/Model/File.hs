{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeFamilies              #-}

module YACP.Model.File
  ( File(..)
  , FileType(..)
  , defaultFileRootIdentifier
  , mkFile
  ) where

import           YACP.Model.Identifier
import           YACP.Model.Licenseable
import           YACP.YacpPrelude

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
import qualified Distribution.SPDX       as SPDX hiding (NONE)
import qualified Distribution.SPDX.Extra as SPDX
import qualified SPDX.Document           as SPDX
import           System.Console.Pretty   (Color (Green), color)
import qualified System.FilePath         as FP
import           System.IO               (hPutStrLn, stderr)
import           System.Random           (randomIO)

--------------------------------------------------------------------------------
{-|
 class for File
-}
data FileType
  = FileType_File
  | FileType_Folder
  deriving (Eq, Generic)

instance A.ToJSON FileType

instance A.FromJSON FileType

instance Show FileType where
  show FileType_File   = "file"
  show FileType_Folder = "folder"

data File =
  File
    { _getFileRootIdentifier  :: Identifier
    , _getFilePath            :: FilePath
    , _getFileType            :: FileType
    , _getFileOtherIdentifier :: Identifier
    , _getFileLicense         :: SPDX.MaybeLicenseExpression
    }
  deriving (Eq, Show, Generic)

instance A.ToJSON File

instance A.FromJSON File

defaultFileRootIdentifier :: Identifier
defaultFileRootIdentifier = PathIdentifier "/"

mkFile :: FilePath -> File
mkFile fp =
  File
    defaultFileRootIdentifier
    fp
    FileType_File
    mempty
    (SPDX.MLicExp SPDX.NOASSERTION)

instance Identifiable File where
  getIdentifier f = PathIdentifier (_getFilePath f) <> _getFileOtherIdentifier f
  addIdentifier f@File {_getFileOtherIdentifier = is} i =
    f {_getFileOtherIdentifier = is <> i}

instance Licenseable File where
  getLicense = _getFileLicense
