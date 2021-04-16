{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.HHC
  ( writeHHC
  , writeHHC',writeHHCFile
  -- for testing
  , HHC_Metadata (..)
  , HHC_Resources (..), countFiles
  , fpToResources, fpsToResources
  , HHC_FrequentLicense (..)
  , HHC_ExternalAttribution (..), HHC_ExternalAttribution_Source (..)
  , HHC (..)
  , computeHHC, writeHHCStats
  ) where

import YACP.Core

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Map as Map
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

data HHC_Metadata
  = HHC_Metadata
  { projectId :: String
  , fileCreationDate :: String
  } deriving (Show, Generic)
instance A.ToJSON HHC_Metadata where
  toJSON (HHC_Metadata pid fcd) = A.object
    [ "projectId" A..= (T.pack pid)
    , "fileCreationDate" A..= (T.pack fcd)
    ]

data HHC_Resources
  = HHC_Resources 
  { _dirs :: (Map.Map FilePath HHC_Resources) 
  , _files :: [FilePath]
  } deriving (Show, Generic, Eq)
instance A.ToJSON HHC_Resources where
    toJSON (HHC_Resources dirs files) = let
        pairsFromDirs = 
            map (\(fragment, resources) -> ((T.pack fragment) A..= (A.toJSON resources)))
            (Map.toList dirs) 
        pairsFromFiles = map (\fragment ->  (T.pack fragment) A..= (1::Int)) files
      in A.object (pairsFromFiles ++ pairsFromDirs)
instance Semigroup HHC_Resources where
  (HHC_Resources dirs1 files1) <> (HHC_Resources dirs2 files2) = let
    dirs = (Map.unionWith (<>) dirs1 dirs2)
    dirNames = Map.keys dirs
    files = (filter (\f -> not $ f `elem` dirNames) $ List.nub (files1 ++ files2))
    in HHC_Resources dirs files
instance Monoid HHC_Resources where
  mempty = HHC_Resources (Map.empty) []
fpToResources :: FileType -> FilePath -> HHC_Resources
fpToResources filetype = let
    fpToResources' :: [FilePath] -> HHC_Resources
    fpToResources' (f : []) = if filetype == FileType_File
                             then HHC_Resources (Map.empty) [f]
                             else HHC_Resources (Map.singleton f mempty) []
    fpToResources' (f : fs) = HHC_Resources (Map.singleton f (fpToResources' fs)) []
  in fpToResources' . (map dropTrailingPathSeparator) . splitPath
fpsToResources :: [FilePath] -> HHC_Resources
fpsToResources = mconcat . map (fpToResources FileType_File)
countFiles :: HHC_Resources -> Int
countFiles (HHC_Resources dirs files) = length files + ((sum . map countFiles . Map.elems) dirs)

data HHC_ExternalAttribution_Source
  = HHC_ExternalAttribution_Source String Double
  deriving (Show, Generic)
instance A.ToJSON HHC_ExternalAttribution_Source where
    toJSON (HHC_ExternalAttribution_Source source documentConfidence) =
        A.object [ "name" A..= (T.pack source)
                 , "documentConfidence" A..= documentConfidence
                 ]

data HHC_ExternalAttribution
  = HHC_ExternalAttribution
  { source :: HHC_ExternalAttribution_Source
  , attributionConfidence :: Double
  , comment :: T.Text
  , originId :: UUID
  , identifier :: Identifier
  , copyright :: T.Text
  , licenseName :: T.Text
  } deriving (Show, Generic)
instance A.ToJSON HHC_ExternalAttribution where
    toJSON (HHC_ExternalAttribution 
        source
        attributionConfidence
        comment
        originId
        identifier
        copyright
        licenseName) = let
            fromIdentifier = \case
                Identifier str -> [ "packageName" A..= str ]
                UuidIdentifier uuid -> [ "packageName" A..= uuid ]
                PathIdentifier fp -> [ "packageName" A..= fp ]
                UrlIdentifier url -> [ "packageName" A..= url ]
                PURL schemeP
                     typeP
                     namespaceP
                     nameP
                     versionP
                     qualifiersP
                     subpathP -> Maybe.catMaybes [ Just $ "packageName" A..= nameP
                                 , fmap ("packageNamespace" A..=) namespaceP
                                 , fmap ("packageType" A..=) typeP
                                 , fmap ("packageVersion" A..=) versionP
                                --  , "packagePURLAppendix" A..= ('?' : qualifiersP ++ ('#' : subpathP)) -- TODO
                     ]
                Hash typeH
                     hashH -> [ "packageName" A..= hashH ]
                Identifiers (i:_) -> fromIdentifier i
                Identifiers [] -> []

          in A.object ([ "source" A..= source
                       , "attributionConfidence" A..= attributionConfidence
                       , "comment" A..= comment
                       , "originId" A..= originId
                       , "copyright" A..= copyright
                       , "licenseName" A..= licenseName
                       ] ++ (fromIdentifier identifier))

data HHC_FrequentLicense
  = HHC_FrequentLicense
  { shortName :: T.Text
  , fullName :: T.Text
  , defaultText :: T.Text
  } deriving (Eq, Show, Generic)
instance A.ToJSON HHC_FrequentLicense where
  toJSON (HHC_FrequentLicense sn fn dt) = A.object [ "shortName" A..= sn
                                                   , "fullName" A..= fn
                                                   , "defaultText"  A..= dt
                                                   ]

data HHC
  = HHC
  { metadata :: Maybe HHC_Metadata
  , resources :: HHC_Resources
  , externalAttributions :: Map.Map UUID HHC_ExternalAttribution
  , resourcesToAttributions :: Map.Map FilePath [UUID]
  , frequentLicenses :: [HHC_FrequentLicense]
  } deriving (Show, Generic)
instance A.ToJSON HHC where
    toJSON (HHC
        metadata
        resources
        externalAttributions
        resourcesToAttributions
        frequentLicenses) = A.object
          [ "metadata" A..= metadata
          , "resources" A..= resources
          , "externalAttributions" A..= externalAttributions
          , "resourcesToAttributions" A..= resourcesToAttributions
          , "frequentLicenses" A..= frequentLicenses
          ]
instance Semigroup HHC where
    hhc1 <> hhc2 = let
          mergedResources = resources hhc1 <> resources hhc2 
          mergedExternalAttributions = Map.union (externalAttributions hhc1) (externalAttributions hhc2)
          mergedResourcesToAttributions = Map.unionWith (++) (resourcesToAttributions hhc1) (resourcesToAttributions hhc2) -- TODO: nub
          mergedFrequentLicenses = List.nub (frequentLicenses hhc1 ++ frequentLicenses hhc2)
        in HHC (metadata hhc1) 
               mergedResources
               mergedExternalAttributions
               mergedResourcesToAttributions
               mergedFrequentLicenses
instance Monoid HHC where
    mempty = HHC Nothing mempty Map.empty Map.empty []

-- ############################################################################

getHhcFromFiles :: YACP HHC
getHhcFromFiles = let
    mkAtt :: (Identifiable a, Licenseable a, Show a) => String -> a -> YACP (UUID, HHC_ExternalAttribution)
    mkAtt source a = do
        uuid <- MTL.liftIO randomIO
        return ( uuid
               , HHC_ExternalAttribution (HHC_ExternalAttribution_Source source 100)
                                             100
                                             (tShow a)
                                             uuid
                                             (getIdentifier a)
                                             ""
                                             (T.pack $ showLicense a)
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
                            , _getFileOtherIdentifier = foi
                            , _getFileLicense = fl
                            }) -> do
        fHhc <- mkAttHhc fp "YACP-File" f
        fCsHhc <- mkHhcFromComponents fp foi 
        fRsHhc <- mkHhcFromRelations fp foi 
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
