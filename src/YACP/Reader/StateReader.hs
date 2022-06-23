{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.StateReader
  ( readStateFile
  ) where

import YACP.MyPrelude
import YACP.Model
import YACP.State

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.State as MTL

readStateFile :: FilePath -> YACP ()
readStateFile = undefined
