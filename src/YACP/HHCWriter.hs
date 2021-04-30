{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.HHCWriter
  ( writeHHC
  , writeHHC',writeHHCFile
  , computeHHC, writeHHCStats
  ) where

import YACP.Core
import YACP.HHC.HHC
import YACP.ComputeGraph

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Map as Map
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Maybe as Maybe
import qualified Data.HashMap.Strict as HM
import qualified Control.Monad.State as MTL
import qualified System.Process as P
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.ByteString.Lazy as B
import Data.UUID (UUID)
import System.Random (randomIO)

getHhcFromFiles :: YACP HHC
getHhcFromFiles = let
    mkAtt :: (Identifiable a, Licenseable a, Show a) => String -> a -> YACP (UUID, HHC_ExternalAttribution)
    mkAtt source a = do
        uuid <- MTL.liftIO randomIO
        return ( uuid
               , HHC_ExternalAttribution (HHC_ExternalAttribution_Source source 100)
                                             100
                                             (Just $ tShow a)
                                             (Just uuid)
                                             (getIdentifier a)
                                             Nothing
                                             (Just . T.pack $ showLicense a)
               )
    mkAttHhc :: (Identifiable a, Licenseable a, Show a) => FilePath -> String -> a -> YACP HHC 
    mkAttHhc fp source a = do
        (uuid, att) <- mkAtt source a
        let absFp = if isAbsolute fp
                    then fp
                    else "/" ++ fp
        return $ HHC Nothing mempty (Map.singleton uuid att) (Map.singleton absFp [uuid]) []
    mkHhcFromComponents :: FilePath -> Identifier -> YACP HHC
    mkHhcFromComponents fp foi = do
        Components csForIdentifier <- getCsForIdentifier foi
        hhcsFromCs <- mapM (mkAttHhc fp "YACP-Component") (V.toList csForIdentifier)
        return (mconcat hhcsFromCs)
    mkHhcFromRelations :: FilePath -> Identifier -> YACP HHC
    mkHhcFromRelations fp foi = let
        mkHhcForOtherIdentifier :: String -> Identifier -> YACP HHC
        mkHhcForOtherIdentifier source otherIdentifier = do
          Components csForIdentifier <- getCsForIdentifier otherIdentifier
          hhcsFromCs <- mapM (mkAttHhc fp source) (V.toList csForIdentifier)
          return (mconcat hhcsFromCs)
        in 
        do
            Relations rsForIdentifier <- getRsForIdentifier foi
            hhcsFromRs <- mapM (\r@(Relation src t target) ->
                if foi `matchesIdentifiable` src
                then if foi `matchesIdentifiable` target
                     then return mempty
                     else mkHhcForOtherIdentifier ("YACP-" ++ show t) target
                else mkHhcForOtherIdentifier ("YACP-" ++ show t ++ "-inv") src
                ) (V.toList rsForIdentifier)
            return (mconcat hhcsFromRs)


    getHhcFromFile :: File -> YACP HHC
    getHhcFromFile = \(f@File {_getFileRootIdentifier = fri
                            , _getFilePath = fp
                            , _getFileType = ft
                            , _getFileLicense = fl
                            }) -> do
        fHhc <- mkAttHhc fp "YACP-File" f
        fCsHhc <- mkHhcFromComponents fp (getIdentifier f)
        fRsHhc <- mkHhcFromRelations fp (getIdentifier f) 
        return (mconcat [(HHC Nothing (fpToResources FileType_File fp) Map.empty Map.empty [])
                        , case fl of 
                            Just expr -> fHhc
                            Nothing -> mempty
                        , fCsHhc
                        , fRsHhc
                        ])
  in MTL.get >>= \(State
         { _getFiles = Files files
         , _getComponents = Components cs
         }) -> do
  hhcs <- mapM getHhcFromFile (V.toList files)
  return (mconcat hhcs) 

computeHHC :: YACP HHC  
computeHHC = MTL.get >>= \(State
                 { _getRoots = roots
                 , _getComponents = Components cs
                 , _getRelations = Relations rs
                 , _getFiles = files
                 }) -> do
    let hhcMetadata = (HHC (Just $ HHC_Metadata "projectId" "fileCreationDate")
                           mempty
                           Map.empty
                           Map.empty
                           [])
    graph <- computeGraphWithDepths
    hhcFromFiles <- getHhcFromFiles

    return (hhcMetadata <> hhcFromFiles)

writeHHCStats :: HHC -> IO ()
writeHHCStats (HHC { metadata = m
                   , resources = rs
                   , externalAttributions = eas
                   , resourcesToAttributions = rtas
                   , frequentLicenses = fls
                   }) = do
                     putStrLn ("metadata: " ++ show m)
                     putStrLn ("resources: #files=" ++ (show (countFiles rs)))
                     putStrLn ("externalAttributions: #=" ++ (show (length eas)))
                     putStrLn ("resourcesToAttributions: #=" ++ (show (length rtas)))
                     putStrLn ("frequentLicenses: #=" ++ (show (length fls)))

writeHHC :: Handle -> YACP ()
writeHHC h = MTL.get >>= \(State
                 { _getRoots = roots
                 , _getComponents = (Components cs)
                 , _getRelations = (Relations rs)
                 }) -> do
  hhc <- computeHHC
  MTL.liftIO $ do
    writeHHCStats hhc
    B.hPutStr h (A.encodePretty hhc)

writeHHC' :: YACP ()
writeHHC' = writeHHC stdout

writeHHCFile :: FilePath -> YACP ()
writeHHCFile fp = do
  stderrLog $ "writeHHCFile " ++ fp
  state <- MTL.get
  MTL.liftIO $ do
    IO.withFile fp IO.WriteMode $
      \h -> runYACP' (writeHHC h) state
  return ()
