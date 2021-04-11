{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.HHC
  ( writeHHC
  , writeHHC',writeHHCFile
  ) where

import YACP.Core

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Control.Monad.State as MTL
import qualified System.Process as P
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.ByteString.Lazy as B
import Data.UUID (UUID)

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
  } deriving (Show, Generic)
instance A.ToJSON HHC_Resources where
    toJSON (HHC_Resources dirs files) = let
        pairsFromDirs = 
            map (\(fragment, resources) -> ((T.pack fragment) A..= (A.toJSON resources)))
            (Map.toList dirs) 
        pairsFromFiles = map (\fragment ->  (T.pack fragment) A..= (1::Int)) files
      in A.object (pairsFromDirs ++ pairsFromFiles)
instance Semigroup HHC_Resources where
  (HHC_Resources dirs1 files1) <> (HHC_Resources dirs2 files2) =
      HHC_Resources (Map.unionWith (<>) dirs1 dirs2) (List.nub (files1 ++ files2))
instance Monoid HHC_Resources where
  mempty = HHC_Resources (Map.empty) []
fpToResources :: Bool -> FilePath -> HHC_Resources
fpToResources isFile = let
    fpToResources' :: [FilePath] -> HHC_Resources
    fpToResources' (f : []) = if isFile 
                             then HHC_Resources (Map.empty) [f]
                             else HHC_Resources (Map.singleton f mempty) []
    fpToResources' (f : fs) = HHC_Resources (Map.singleton f (fpToResources' fs)) []
  in fpToResources' . splitPath --  . map dropTrailingPathSeparator -- TODO
fpsToResources :: [FilePath] -> HHC_Resources
fpsToResources = mconcat . map (fpToResources True)

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
  , licenseText :: T.Text
  } deriving (Show, Generic)
instance A.ToJSON HHC_ExternalAttribution where
    toJSON (HHC_ExternalAttribution 
        source
        attributionConfidence
        comment
        originId
        identifier
        copyright
        licenseText) = let
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
                     subpathP -> [ "packageName" A..= nameP
                                 , "packageNamespace" A..= namespaceP
                                 , "packageType" A..= typeP
                                 , "packageVersion" A..= versionP
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
                       , "licenseText" A..= licenseText
                       ] ++ (fromIdentifier identifier))

data HHC_ResourcesToAttributions
  = HHC_ResourcesToAttributions (Map.Map FilePath [UUID])
   deriving (Show, Generic)
instance A.ToJSON HHC_ResourcesToAttributions where
    toJSON (HHC_ResourcesToAttributions map) = A.toJSON map

data HHC_FrequentLicense
  = HHC_FrequentLicense
  { shortName :: T.Text
  , fullName :: T.Text
  , defaultText :: T.Text
  } deriving (Show, Generic)
instance A.ToJSON HHC_FrequentLicense where
  toJSON (HHC_FrequentLicense sn fn dt) = A.object [ "shortName" A..= sn
                                                   , "fullName" A..= fn
                                                   , "defaultText"  A..= dt
                                                   ]

data HHC
  = HHC
  { metadata :: HHC_Metadata
  , resources :: HHC_Resources
  , externalAttributions :: Map.Map UUID HHC_ExternalAttribution
  , resourcesToAttributions :: HHC_ResourcesToAttributions 
  , frequentLicenses :: [HHC_FrequentLicense]
  } deriving (Show, Generic)
instance A.ToJSON HHC where
    toJSON (HHC
        metadata
        resources
        externalAttributions
        resourcesToAttributions
        frequentLicenses) = A.object
          [  "metadata" A..= metadata
          , "resources" A..= resources
          , "externalAttributions" A..= externalAttributions
          , "resourcesToAttributions" A..= resourcesToAttributions
          , "frequentLicenses" A..= frequentLicenses
          ]

-- ############################################################################

filesToResources :: Files -> HHC_Resources
filesToResources (Files files) = mconcat . V.toList $ V.map (fpToResources True . _getFilePath) files

computeHHC :: YACP HHC  
computeHHC = MTL.get >>= \(State
                 { _getRoots = roots
                 , _getComponents = (Components cs)
                 , _getRelations = (Relations rs)
                 , _getFiles = files
                 }) -> do
    let resourcesFromFiles = filesToResources files
    return (HHC 
      (HHC_Metadata "projectId" "fileCreationDate")
      resourcesFromFiles
      Map.empty
      (HHC_ResourcesToAttributions Map.empty)
      []
      )

writeHHC :: Handle -> YACP ()
writeHHC h = MTL.get >>= \(State
                 { _getRoots = roots
                 , _getComponents = (Components cs)
                 , _getRelations = (Relations rs)
                 }) -> do
  hhc <- computeHHC
  MTL.liftIO $ do
    B.hPutStr h (A.encode hhc)

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
