{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Model.File
  ( FileType
  , File (..)
  , defaultFileRootIdentifier
  , mkFile
  , Files (..)
  ) where

import YACP.MyPrelude
import YACP.Model.Identifier
import YACP.Model.License

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

data FileType = FileType_File | FileType_Folder
  deriving (Eq, Generic)
instance A.ToJSON FileType
instance A.FromJSON FileType
instance Show FileType where
  show FileType_File = "file"
  show FileType_Folder = "folder"
data File
  = File
  { _getFileRootIdentifier :: Identifier
  , _getFilePath :: FilePath
  , _getFileType :: FileType
  , _getFileOtherIdentifier :: Identifier
  , _getFileLicense :: MaybeLicenseExpression
  } deriving (Eq, Show, Generic)
instance A.ToJSON File
instance A.FromJSON File
defaultFileRootIdentifier :: Identifier
defaultFileRootIdentifier = PathIdentifier "/"
mkFile :: FilePath -> File
mkFile fp = File defaultFileRootIdentifier fp FileType_File mempty mempty

instance Identifiable File where
  getIdentifier f = PathIdentifier (_getFilePath f) <> _getFileOtherIdentifier f
  addIdentifier f@File{_getFileOtherIdentifier = is} i = f{_getFileOtherIdentifier = is<>i}

instance Licenseable File where
  getLicense = _getFileLicense

data Files
  = Files (Vector File)
  deriving (Eq, Show, Generic)
instance A.ToJSON Files
instance A.FromJSON Files
instance Semigroup Files where
    (Files fs1) <> (Files fs2) = Files (vNub (fs1 <> fs2))
instance Monoid Files where
    mempty = Files mempty