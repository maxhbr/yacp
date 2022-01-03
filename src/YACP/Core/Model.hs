{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Core.Model
  ( module X
  -- Identifier
  , Identifier (..)
  , matchesIdentifier
  , flattenIdentifierToList
  , mkUUID
  , parsePURL
  , Identifiable (..)
  -- Component
  , Component (..)
  , identifierToComponent
  , Licenseable (..)
  , renderSpdxLicense
  -- Relations
  , RelationType (..)
  , Relation (..)
  , relationContainsIdentifier
  -- File
  , File (..), FileType (..)
  , defaultFileRootIdentifier, mkFile
  -- State
  , State (..), Components (..), Relations (..), Files (..), YACPIssue (..)
  , YACP (..), runYACP, runYACP'
  ) where

import YACP.Core.MyPrelude as X
import SPDX.Document.RelationshipTypes as X
import qualified PURL.PURL as PURL

import System.Console.Pretty (color, Color(Green))
import System.IO (hPutStrLn, stderr)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, maybeToList)
import Data.UUID (UUID)
import System.Random (randomIO)
import Data.String (IsString(..))
import qualified Control.Monad.State as MTL
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Monoid (mconcat)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Distribution.SPDX as SPDX
import qualified Distribution.SPDX.Extra as SPDX
import qualified Distribution.SPDX.License as SPDX
import qualified Distribution.Parsec as SPDX
import qualified Network.URI as URI
import qualified System.FilePath as FP


--------------------------------------------------------------------------------
{-|
  Class for Identifier
-}
data Identifier
  = Identifier String
  | UuidIdentifier UUID
  | PathIdentifier FilePath -- path of file
  | UrlIdentifier String
  | PURL PURL.PURL
  | Hash (Maybe String) -- type
         String         -- hash
  -- | CPE -- TODO
  | Identifiers [Identifier] -- the best one is the head
  deriving (Eq, Generic)
instance A.ToJSON Identifier
instance A.FromJSON Identifier
instance IsString Identifier where
    fromString = Identifier
instance Show Identifier where
  show (Identifier str) = str
  show (UuidIdentifier uuid) = show uuid
  show (UrlIdentifier url) = url
  show purl = show uri
  show (PathIdentifier fp) = fp
  show (Hash (Just t) h) = t ++ ":" ++ h
  show (Hash Nothing h) = h
  show (Identifiers [i]) = show i
  show (Identifiers is) = show is
flattenIdentifierToList :: Identifier -> [Identifier]
flattenIdentifierToList (Identifiers is) = nub $ concatMap flattenIdentifierToList is
flattenIdentifierToList i                = [i]

mkUUID :: IO Identifier
mkUUID = do
  uuid <- randomIO
  return (UuidIdentifier uuid)

parsePURL :: String -> Identifier
parsePURL uriStr = case PURL.parsePURL uriStr of
  Just purl -> PURL purl
  Nothing  -> Identifier uriStr

instance Semigroup Identifier where
  i1 <> (Identifiers []) = i1
  (Identifiers []) <> i2 = i2
  i1 <> i2               = let
    i1List = flattenIdentifierToList i1
    i2List = flattenIdentifierToList i2
    in Identifiers (nub $ i1List ++ i2List)
instance Monoid Identifier where
  mempty = Identifiers []

matchesIdentifier :: Identifier -> Identifier -> Bool
matchesIdentifier i1 i2 = let
    matchesIdentifier' :: Identifier -> Identifier -> Bool
    matchesIdentifier' i1' i2' = i1' == i2' -- TODO: better matching? wildcards?
    i1s = flattenIdentifierToList i1
    i2s = flattenIdentifierToList i2
    product = [(a, b) | a <- i1s, b <- i2s]
  in any (uncurry matchesIdentifier') product


{-|
  Class for Identifiable
-}
class Identifiable a where
  getIdentifier :: a -> Identifier

  addIdentifier :: a -> Identifier -> a

  addUuidIfMissing :: a -> IO a
  addUuidIfMissing a = let
    is = flattenIdentifierToList (getIdentifier a)
    hasIdentifiers = not (null is) :: Bool
    in if hasIdentifiers
       then return a
       else do
    uuidID <- mkUUID
    return (a `addIdentifier` uuidID)

  matchesIdentifiable :: Identifier -> a -> Bool
  matchesIdentifiable i a = i `matchesIdentifier` (getIdentifier a)
instance Identifiable Identifier where
  getIdentifier = id
  addIdentifier = (<>)

--------------------------------------------------------------------------------
instance A.ToJSON SPDX.LicenseExpression where
  toJSON expression = A.String $ tShow expression
instance A.FromJSON SPDX.LicenseExpression where
  parseJSON = A.withText "SPDX.LicenseExpression" $ \t -> 
    case SPDX.eitherParsec (T.unpack t) of 
      Right exp -> return $ (`SPDX.ELicense` Nothing) exp
      Left err -> fail err

--------------------------------------------------------------------------------
{-|
  Class for Component
-}
data Component
  = Component
  { _getComponentIdentifier :: Identifier
  , _getComponentLicense :: Maybe SPDX.LicenseExpression
  , _getComponentPayload :: A.Array
  , _getComponentRelations :: [Relation]
  , _getComponentSubComponents :: [Component]
  } deriving (Eq, Generic)
instance A.ToJSON Component
instance A.FromJSON Component
instance Show Component where
  show (Component{_getComponentIdentifier = cId, _getComponentLicense = l}) = "{{{" ++  show cId ++ "@" ++ show l ++ "}}}"
instance Identifiable Component where
  getIdentifier = _getComponentIdentifier
  addIdentifier (c@Component{_getComponentIdentifier = is}) i = c{_getComponentIdentifier = is<>i}
instance Semigroup Component where
  c1 <> c2 = let
    mergedIdentifiers = (getIdentifier c1) <> (getIdentifier c2)
    mergedLicense = let
       l1 = _getComponentLicense c1
       l2 = _getComponentLicense c2
      in case l1 of
        Nothing  -> l2
        Just l1' -> case l2 of
          Nothing  -> l1
          Just l2' -> Just (l1' `SPDX.EOr` l2')
    mergedPayload = let
      p1 = _getComponentPayload c1
      p2 = _getComponentPayload c2
      in if p1 /= p2
         then p1 <> p2
         else p1
    mergedRelations = let
      r1 = _getComponentRelations c1
      r2 = _getComponentRelations c2
      in nub (r1 ++ r2)
    mergedSubComponents = let
      sc1 = _getComponentSubComponents c1
      sc2 = _getComponentSubComponents c2
      in nub (sc1 ++ sc2)
    in Component
       { _getComponentIdentifier = mergedIdentifiers
       , _getComponentLicense = mergedLicense
       , _getComponentPayload = mergedPayload
       , _getComponentRelations = mergedRelations
       , _getComponentSubComponents = mergedSubComponents
       }
instance Monoid Component where
  mempty = Component mempty Nothing mempty mempty mempty

identifierToComponent :: Identifier -> Component
identifierToComponent i = mempty{_getComponentIdentifier = i}

class Licenseable a where
  getLicense :: a -> Maybe SPDX.LicenseExpression
  showLicense :: a -> String
  showLicense a = case getLicense a of
    Just l -> renderSpdxLicense l
    Nothing -> ""

instance Licenseable Component where
  getLicense = _getComponentLicense

data Relation
  = Relation
  { _getRelationSrc :: Identifier
  , _getRelationType :: RelationType
  , _getRelationTarget :: Identifier
  } deriving (Eq, Generic)
instance A.ToJSON Relation
instance A.FromJSON Relation
instance Show Relation where
  show (Relation rSrc rType rTarget) = "{{{" ++ show rSrc ++ " >" ++ show rType ++ "> " ++ show rTarget ++ "}}}"

relationContainsIdentifier :: Identifiable a => a -> Relation -> Bool
relationContainsIdentifier a (Relation src _ target) = any (`matchesIdentifiable` a) [src, target]

{-|
  direction should always be from the smaller to the bigger in which it is included
-}

--------------------------------------------------------------------------------
{-|
 class for File
-}
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
  , _getFileLicense :: Maybe SPDX.LicenseExpression
  } deriving (Eq, Show, Generic)
instance A.ToJSON File
instance A.FromJSON File
defaultFileRootIdentifier :: Identifier
defaultFileRootIdentifier = PathIdentifier "/"
mkFile :: FilePath -> File
mkFile fp = File defaultFileRootIdentifier fp FileType_File mempty Nothing

instance Identifiable File where
  getIdentifier f = PathIdentifier (_getFilePath f) <> _getFileOtherIdentifier f
  addIdentifier f@File{_getFileOtherIdentifier = is} i = f{_getFileOtherIdentifier = is<>i}

instance Licenseable File where
  getLicense = _getFileLicense

--------------------------------------------------------------------------------

data Components
  = Components (Vector Component)
  deriving (Eq, Show, Generic)
instance A.ToJSON Components
instance A.FromJSON Components

data Relations
  = Relations (Vector Relation)
  deriving (Eq, Show, Generic)
instance A.ToJSON Relations
instance A.FromJSON Relations

data Files
  = Files (Vector File)
  deriving (Eq, Show, Generic)
instance A.ToJSON Files
instance A.FromJSON Files

data YACPIssue
  = YACPParsingIssue String
  | YACPFileParsingIssue FilePath String
  deriving (Eq, Show, Generic)
instance A.ToJSON YACPIssue
instance A.FromJSON YACPIssue

data State
  = State
  { _getRoots :: [Identifier]
  , _getComponents :: Components
  , _getRelations :: Relations
  , _getFiles :: Files  
  , _getYacpIssues :: [YACPIssue]
  } deriving (Eq, Show, Generic)
instance A.ToJSON State
instance A.FromJSON State

type YACP a
  = MTL.StateT State IO a
runYACP :: YACP a -> IO (a, State)
runYACP yacp = let
  initialState = State [] (Components V.empty) (Relations V.empty) (Files V.empty) []
  in runYACP' yacp initialState
runYACP' :: YACP a -> State -> IO (a, State)
runYACP' yacp initialState = MTL.runStateT yacp initialState
