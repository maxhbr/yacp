{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
module YACP.Core.Model
  ( module X
  , module PURL
  -- Identifier
  , Identifier (..)
  , matchesIdentifier
  , flattenIdentifierToList
  , mkUUID
  , parsePURL
  , nameAndVersion
  , Identifiable (..)
  -- Relations
  , RelationType (..)
  , Relation (..)
  , relationContainsIdentifier
  , normalizeRelation
  , Relations (..)
  -- Statement
  , Origin (..)
  , StatementMetadata (..)
  , StatementContent (..)
  , Statemental (..)
  , Statement (..)
  , setOirigin1
  , Statements (..)
  , setOirigin
  , clusterifyStatements
  ) where

import YACP.Core.MyPrelude
import SPDX.Document.RelationshipTypes as X
import SPDX.LicenseExpression as X
import PURL.PURL hiding (parsePURL)
import qualified PURL.PURL as PURL

import System.Console.Pretty (color, Color(Green))
import System.IO (hPutStrLn, stderr)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, maybeToList, catMaybes)
import Data.UUID (UUID)
import qualified Data.Map                      as Map
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
import Data.Typeable
import Data.Dynamic


--------------------------------------------------------------------------------
data Identifier
  = Identifier String
  | UuidIdentifier UUID
  | AbsolutePathIdentifier FilePath -- path of file
  | RelativePathIdentifier Identifier FilePath
  | UrlIdentifier String
  | PurlIdentifier PURL.PURL -- all coordinates should be converted to purls
  | Hash (Maybe String) -- type
         String         -- hash
  -- | CPE -- TODO, (sometimes) not a good Identifier?
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
  show (PurlIdentifier purl) = show purl
  show (AbsolutePathIdentifier fp) = fp
  show (RelativePathIdentifier _ fp) = fp
  show (Hash (Just t) h) = t ++ ":" ++ h
  show (Hash Nothing h) = h
  show (Identifiers [i]) = show i
  show (Identifiers is) = show is
flattenIdentifierToList :: Identifier -> [Identifier]
flattenIdentifierToList (Identifiers is) = nub $ concatMap flattenIdentifierToList is
flattenIdentifierToList i                = [i]

nameAndVersion :: String -> String -> Identifier
nameAndVersion name version = PurlIdentifier (PURL (Just "pkg") Nothing Nothing name (Just version) Nothing Nothing)

mkUUID :: IO Identifier
mkUUID = do
  uuid <- randomIO
  return (UuidIdentifier uuid)

parsePURL :: String -> Identifier
parsePURL uriStr = case PURL.parsePURL uriStr of
  Just purl -> PurlIdentifier purl
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
    matchesIdentifier' (Hash _ h1) (Hash _ h2) = h1 == h2 -- ignore type
    matchesIdentifier' (PurlIdentifier p1) (PurlIdentifier p2) = 
      and [ PURL._PURL_type p1 == PURL._PURL_type p2 
          , PURL._PURL_namespace p1 == PURL._PURL_namespace p2 
          , PURL._PURL_name p1 == PURL._PURL_name p2       
          , PURL._PURL_version p1 == PURL._PURL_version p2
          ] -- ignore qualifiers and subpath
    matchesIdentifier' i1' i2' = i1' == i2' -- TODO: better matching? wildcards? path sanitation?

    i1s = flattenIdentifierToList i1
    i2s = flattenIdentifierToList i2
    product = [(a, b) | a <- i1s, b <- i2s]
  in any (uncurry matchesIdentifier') product

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

type Identifiers = V.Vector Identifier
class IdentifierProvider a where
  getIdentifiers :: a -> Identifiers
  getIdentifierClusters :: a -> Identifiers
  getIdentifierClusters = let
    clusterifyIdentifiers :: Identifiers -> Identifiers
    clusterifyIdentifiers = let
        fun :: Identifiers -> Identifier -> Identifiers
        fun acc i = case V.uncons acc of 
          Just (i', acc') -> if i `matchesIdentifier` i'
                            then (i <> i') `V.cons` acc'
                            else i' `V.cons` (fun acc' i)
          Nothing -> V.singleton i
      in V.foldl' fun V.empty
    in clusterifyIdentifiers . getIdentifiers
instance IdentifierProvider a => IdentifierProvider (Maybe a) where
  getIdentifiers (Just a) = getIdentifiers a
  getIdentifiers Nothing  = mempty

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
instance IdentifierProvider Relation where
  getIdentifiers (Relation src _ target) = V.fromList [src, target]

relationContainsIdentifier :: Identifiable a => a -> Relation -> Bool
relationContainsIdentifier a (Relation src _ target) = any (`matchesIdentifiable` a) [src, target]

{-|
  direction should always be from the smaller to the bigger in which it is included
-}
normalizeRelation :: Relation -> Relation
normalizeRelation = let
    flipDirection :: Relation -> Relation
    flipDirection (r@Relation { _getRelationSrc = src
                              , _getRelationTarget = target
                              }) =
      r {_getRelationSrc = target, _getRelationTarget = src}
    normalizeRelation' :: Relation -> Relation
    normalizeRelation' (r@Relation {_getRelationType = DEPENDS_ON}) =
      flipDirection (r {_getRelationType = DEPENDENCY_OF})
    normalizeRelation' (r@Relation {_getRelationType = DESCRIBED_BY}) =
      flipDirection (r {_getRelationType = DESCRIBES})
    normalizeRelation' (r@Relation {_getRelationType = CONTAINS}) =
      flipDirection (r {_getRelationType = CONTAINED_BY})
    normalizeRelation' (r@Relation {_getRelationType = HAS_PREREQUISITE}) =
      flipDirection (r {_getRelationType = PREREQUISITE_FOR})
    normalizeRelation' (r@Relation {_getRelationType = GENERATED_FROM}) =
      flipDirection (r {_getRelationType = GENERATES})
    normalizeRelation' r = r
  in normalizeRelation'

data Relations
  = Relations (Vector Relation)
  deriving (Eq, Show, Generic)
instance A.ToJSON Relations
instance A.FromJSON Relations
instance Semigroup Relations where
    (Relations rs1) <> (Relations rs2) = Relations (vNub (rs1 <> rs2))
instance Monoid Relations where
    mempty = Relations mempty
instance IdentifierProvider Relations where
  getIdentifiers (Relations rs) = V.concatMap getIdentifiers rs

normalizeRelations :: Relations -> Relations
normalizeRelations (Relations rs) = Relations (V.map normalizeRelation rs)

data Origin = Origin
  { _getOriginIdentifier :: Identifier
  , _getoriginProvider :: String
  } deriving (Eq, Generic)
instance A.ToJSON Origin
instance A.FromJSON Origin
instance Show Origin where
  show (Origin{_getOriginIdentifier = oId}) = "{{{" ++  show oId ++ "}}}"
instance Identifiable Origin where
  getIdentifier = _getOriginIdentifier
  addIdentifier (c@Origin{_getOriginIdentifier = is}) i = c{_getOriginIdentifier = is<>i}
instance IdentifierProvider Origin where
  getIdentifiers (Origin i _) = V.singleton i

data StatementMetadata = StatementMetadata Identifier (Maybe Origin)
    deriving (Eq, Show, Generic)
instance A.ToJSON StatementMetadata
instance A.FromJSON StatementMetadata
instance IdentifierProvider StatementMetadata where
  getIdentifiers (StatementMetadata i o) = i `V.cons` (getIdentifiers o)

class (IdentifierProvider a) => Statemental a where
    getStatementMetadata :: a -> StatementMetadata
    getStatementSubject :: a -> Identifier
    getStatementSubject a = case  getStatementMetadata a of
        StatementMetadata i _ -> i
    getStatementOrigin :: a -> Maybe Origin
    getStatementOrigin a = case  getStatementMetadata a of
        StatementMetadata _ o -> o
    getStatementRelations' :: a -> Relations
    getStatementRelations' _ = mempty
    getStatementRelations :: a -> Relations
    getStatementRelations = normalizeRelations . getStatementRelations'

-- TODO: would be better to be extensible and not to have a full list here
data StatementContent
    = FoundManifestFile Identifier
    | FoundDependent Identifier
    | FoundDependency Identifier
    | ComponentUrl String
    | ComponentLicense MaybeLicenseExpression
    | ComponentVulnerability String
    deriving (Eq, Show, Generic)
instance A.ToJSON StatementContent
instance A.FromJSON StatementContent
instance IdentifierProvider StatementContent where
  getIdentifiers (FoundManifestFile i) = V.singleton i
  getIdentifiers _ = mempty

data Statement 
  = Statement StatementMetadata TypeRep Dynamic
  deriving (Show, Generic)
instance A.ToJSON Statement
instance A.FromJSON Statement

mkStatement :: Typeable a => StatementMetadata -> a -> Statement
mkStatement sm x = Statement sm (typeOf x) (toDyn x)

unpackStatement :: forall a . Typeable a => Statement -> IO (Maybe a)
unpackStatement (Statement _ have dyn) = let
    want = typeRep (Proxy :: Proxy a)
  in if want == have
     then case fromDynamic dyn of 
        Just x -> pure $ Just x
        _      -> undefined
     else pure Nothing

setOirigin1 :: Origin -> Statement -> Statement
setOirigin1 o (Statement (StatementMetadata i _) t d) = Statement (StatementMetadata i (Just o)) t d

instance IdentifierProvider Statement where
  getIdentifiers s@(Statement sm _ dyn) = let
      identifiersFromMetatada = getIdentifiers sm
      identifiersFromContent = case fromDynamic dyn of
        Just x -> getIdentifiers x
        Nothing -> mempty
      identifiersFromRelations = getIdentifiers (getStatementRelations s)
    in identifiersFromMetatada <> identifiersFromContent <> identifiersFromRelations

instance Statemental Statement where
    getStatementMetadata (Statement sm _ _) = sm

    getStatementRelations' s@(Statement _ (FoundDependent src)) = Relations . pure $ Relation src DEPENDS_ON (getStatementSubject s)
    getStatementRelations' s@(Statement _ (FoundDependency trg)) = Relations . pure $ Relation (getStatementSubject s) DEPENDS_ON trg
    getStatementRelations' _ = mempty

data Statements
  = Statements (Vector Statement)
  deriving (Show, Generic)
instance A.ToJSON Statements
instance A.FromJSON Statements
instance Semigroup Statements where
    (Statements rs1) <> (Statements rs2) = Statements (vNub (rs1 <> rs2))
instance Monoid Statements where
    mempty = Statements mempty
instance IdentifierProvider Statements where
  getIdentifiers (Statements ss) = V.concatMap getIdentifiers ss

unpackStatements :: forall a . Typeable a => Statements -> IO [a]
unpackStatements (Statements ss) = do
  xs <- mapM unpackStatement (V.toList ss)
  return (catMaybes xs)

setOirigin :: Origin -> Statements -> Statements
setOirigin o (Statements ss) = Statements (V.map (setOirigin1 o) ss)

clusterifyStatements :: Statements -> V.Vector (Identifier, Statements)
clusterifyStatements (ss@(Statements ss')) = let
    iClusters = getIdentifierClusters ss
    fun ::  Vector (Identifier, Statements) -> Identifier -> Vector (Identifier, Statements)
    fun prev i = let
        matchingStatements = V.filter (\s -> getStatementSubject s `matchesIdentifier` i) ss'
      in if V.null matchingStatements
         then prev
         else (i,Statements matchingStatements) `V.cons` prev
  in V.foldl' fun V.empty iClusters

type Collector = IO Statements
type Writer = Statements -> IO ()