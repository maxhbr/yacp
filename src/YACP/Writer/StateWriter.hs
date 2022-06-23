{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Writer.StateWriter
  ( writeState
  , writeState'
  , writeStateFile
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
    IO.withFile fp IO.WriteMode $ \h -> runYACP' (writeState h) state
  return ()
