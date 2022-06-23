{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Writer.StateWriter
  ( writeState
  , writeState',writeStateFile
  ) where

import YACP.Core

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as B
import qualified Control.Monad.State as MTL

writeState :: Handle -> YACP ()
writeState h = MTL.get >>= \state -> do
  MTL.liftIO $ do
    B.hPutStr h (A.encodePretty state)

writeState' :: YACP ()
writeState' = writeState stdout

writeStateFile :: FilePath -> YACP ()
writeStateFile fp = do
  stderrLog $ "writeStateFile " ++ fp
  state <- MTL.get
  MTL.liftIO $ do
    IO.withFile fp IO.WriteMode $
      \h -> runYACP' (writeState h) state
  return ()
