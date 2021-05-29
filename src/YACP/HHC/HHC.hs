{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.HHC.HHC
  ( HHC_Metadata (..)
  , HHC_Resources (..), countFiles
  , fpToResources, fpsToResources
  , HHC_FrequentLicense (..)
  , HHC_ExternalAttribution (..), HHC_ExternalAttribution_Source (..)
  , HHC (..)
  ) where

import YACP.Core

import System.IO (Handle, hPutStrLn, hClose, stdout)
import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Map as Map
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
instance A.FromJSON HHC_Metadata where
  parseJSON = A.withObject "HHC_FrequentLicense" $ \v -> do
    HHC_Metadata <$> v A..: "projectId"
                 <*> v A..: "fileCreationDate"

data HHC_Resources
  = HHC_Resources 
  { _dirs :: Map.Map FilePath HHC_Resources
  , _files :: Set.Set FilePath
  } deriving (Show, Generic, Eq)
instance A.ToJSON HHC_Resources where
    toJSON (HHC_Resources dirs files) = let
        pairsFromDirs = 
            map (\(fragment, resources) -> ((T.pack fragment) A..= (A.toJSON resources)))
            (Map.toList dirs) 
        pairsFromFiles = map (\fragment ->  (T.pack fragment) A..= (1::Int)) $ Set.toList files
      in A.object (pairsFromFiles ++ pairsFromDirs)
instance A.FromJSON HHC_Resources where
  parseJSON = A.withObject "HHC_Resources" $ \v -> let
      isObject :: A.Value -> Bool
      isObject (A.Object _) = True
      isObject _            = False
      getFiles :: [(FilePath, A.Value)] -> Set.Set FilePath
      getFiles = Set.fromList . map (\(fp,_) -> fp) . (filter (\(_,v') -> not (isObject v')))
      getDirs :: [(FilePath, A.Value)] -> A.Parser (Map.Map FilePath HHC_Resources)
      getDirs list = do
        parsedList <- mapM (\(fp,o) -> do 
          rs <- A.parseJSON o :: A.Parser HHC_Resources
          return (fp,rs)) (filter (\(_,v') -> isObject v') list)
        return (Map.fromList parsedList)
    in do
      let vList = map (\(t,v') -> (T.unpack t, v')) $ HM.toList v
      dirs <- getDirs vList
      return (HHC_Resources dirs (getFiles vList))
instance Semigroup HHC_Resources where
  (HHC_Resources dirs1 files1) <> (HHC_Resources dirs2 files2) = let
    dirs = (Map.unionWith (<>) dirs1 dirs2)
    dirNames = Map.keys dirs
    files = Set.filter (\f -> not $ f `elem` dirNames) $ files1 <> files2
    in HHC_Resources dirs files
instance Monoid HHC_Resources where
  mempty = HHC_Resources Map.empty Set.empty
fpToResources :: FileType -> FilePath -> HHC_Resources
fpToResources filetype = let
    fpToResources' :: [FilePath] -> HHC_Resources
    fpToResources' (f : []) = if filetype == FileType_File
                             then HHC_Resources (Map.empty) (Set.singleton f)
                             else HHC_Resources (Map.singleton f mempty) Set.empty
    fpToResources' (f : fs) = HHC_Resources (Map.singleton f (fpToResources' fs)) Set.empty
  in fpToResources' . (map dropTrailingPathSeparator) . splitPath
fpsToResources :: [FilePath] -> HHC_Resources
fpsToResources = mconcat . map (fpToResources FileType_File)
countFiles :: HHC_Resources -> Int
countFiles (HHC_Resources dirs files) = length files + ((sum . map countFiles . Map.elems) dirs)

data HHC_ExternalAttribution_Source
  = HHC_ExternalAttribution_Source String Double
  deriving (Show, Generic, Eq)
instance A.ToJSON HHC_ExternalAttribution_Source where
    toJSON (HHC_ExternalAttribution_Source source documentConfidence) =
        A.object [ "name" A..= (T.pack source)
                 , "documentConfidence" A..= documentConfidence
                 ]
instance A.FromJSON HHC_ExternalAttribution_Source where
    parseJSON = A.withObject "HHC_ExternalAttribution_Source" $ \v -> do
        HHC_ExternalAttribution_Source <$> v A..: "name"
                                       <*> v A..: "documentConfidence"

data HHC_ExternalAttribution
  = HHC_ExternalAttribution
  { _source :: HHC_ExternalAttribution_Source
  , _attributionConfidence :: Double
  , _comment :: Maybe T.Text
  , _originId :: Maybe UUID
  , _identifier :: Identifier
  , _copyright :: Maybe T.Text
  , _licenseName :: Maybe T.Text
  , _preselected :: Bool
  } deriving (Show, Generic, Eq)
instance A.ToJSON HHC_ExternalAttribution where
    toJSON (HHC_ExternalAttribution 
        source
        attributionConfidence
        comment
        originId
        identifier
        copyright
        licenseName
        preselected) = let
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
            maybePreselected = if preselected
              then Just True
              else Nothing
          in objectNoNulls ([ "source" A..= source
                            , "attributionConfidence" A..= attributionConfidence
                            , "comment" A..= comment
                            , "copyright" A..= copyright
                            , "licenseName" A..= licenseName
                            , "originId" A..= originId
                            , "preSelected" A..= maybePreselected
                            ] ++ (fromIdentifier identifier))
instance A.FromJSON HHC_ExternalAttribution where
  parseJSON = A.withObject "HHC_ExternalAttribution" $ \v -> let 
      getIdentifierFromJSON = do
        packageName <- v A..:? "packageName"
        packageNamespace <- v A..:? "packageNamespace"
        packageType <- v A..:? "packageType"
        packageVersion <- v A..:? "packageVersion"
        return $ case packageName of 
          Just packageName' -> PURL Nothing packageType packageNamespace packageName' packageVersion Nothing Nothing
          Nothing           -> Identifiers [] -- TODO?
    in do
    source <- v A..: "source"
    attributionConfidence <- fmap (100 `Maybe.fromMaybe`) (v A..:? "attributionConfidence")
    comment <- v A..:? "comment"
    originId <- v A..:? "originId"
    identifier <- getIdentifierFromJSON
    copyright <- v A..:? "copyright"
    licenseName <- fmap (\case
      Just "" -> Nothing
      l -> l) (v A..:? "licenseName")
    preselected <- fmap (False `Maybe.fromMaybe`) (v A..:? "preSelected")

    return (HHC_ExternalAttribution source attributionConfidence comment originId identifier copyright licenseName preselected)

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
instance A.FromJSON HHC_FrequentLicense where
  parseJSON = A.withObject "HHC_FrequentLicense" $ \v -> do
    HHC_FrequentLicense <$> v A..: "shortName"
                        <*> v A..: "fullName"
                        <*> v A..: "defaultText"

data HHC
  = HHC
  { _metadata :: Maybe HHC_Metadata
  , _resources :: HHC_Resources
  , _externalAttributions :: Map.Map UUID HHC_ExternalAttribution
  , _resourcesToAttributions :: Map.Map FilePath [UUID]
  , _frequentLicenses :: [HHC_FrequentLicense]
  } deriving (Show, Generic)
instance A.ToJSON HHC where
    toJSON (HHC
        metadata
        resources
        externalAttributions
        resourcesToAttributions
        frequentLicenses) = objectNoNulls
          [ "metadata" A..= metadata
          , "resources" A..= resources
          , "externalAttributions" A..= externalAttributions
          , "resourcesToAttributions" A..= resourcesToAttributions
          , "frequentLicenses" A..= frequentLicenses
          ]
instance A.FromJSON HHC where
  parseJSON = A.withObject "HHC" $ \v -> do
    resources <- v A..: "resources"
    externalAttributions <- v A..: "externalAttributions"
    resourcesToAttributions <- v A..: "resourcesToAttributions"
    HHC <$> v A..:? "metadata"
        <*> (pure resources)
        <*> (pure externalAttributions)
        <*> (pure resourcesToAttributions)
        <*> (fmap (\case 
            Just fls -> fls
            Nothing -> []) (v A..:? "frequentLicenses"))
instance Semigroup HHC where
    hhc1 <> hhc2 = let
          mergedResources = _resources hhc1 <> _resources hhc2 
          mergedExternalAttributions = Map.union (_externalAttributions hhc1) (_externalAttributions hhc2)
          mergedResourcesToAttributions = Map.unionWith (++) (_resourcesToAttributions hhc1) (_resourcesToAttributions hhc2) -- TODO: nub
          mergedFrequentLicenses = List.nub (_frequentLicenses hhc1 ++ _frequentLicenses hhc2)
        in HHC (_metadata hhc1) 
               mergedResources
               mergedExternalAttributions
               mergedResourcesToAttributions
               mergedFrequentLicenses
instance Monoid HHC where
    mempty = HHC Nothing mempty Map.empty Map.empty []
