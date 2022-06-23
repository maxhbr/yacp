{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.ComponentDetectionReader
  ( readComponentDetectionFile
  , readComponentDetectionBS
  ) where

import YACP.Core

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.State as MTL

readComponentDetectionBS :: B.ByteString -> YACP (Maybe YACPIssue)
readComponentDetectionBS = undefined

readComponentDetectionFile :: FilePath -> YACP ()
readComponentDetectionFile = undefined