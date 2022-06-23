{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Reader.StateReader
  ( readStateFile
  ) where

import           YACP.Core

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.Aeson.Types              as A
import qualified Data.ByteString.Lazy          as B
import           System.IO                      ( Handle
                                                , hClose
                                                , hPutStrLn
                                                , stdout
                                                )
import qualified System.IO                     as IO

readStateFile :: FilePath -> YACP ()
readStateFile = undefined
