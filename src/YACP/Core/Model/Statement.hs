
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}
module YACP.Core.Model.Statement
  ( Origin (..)
  , StatementMetadata (..)
  , Statemental (..)
  , StatementContental (..)
  , Statement (..)
  , setOirigin1
  , Statements (..)
  , setOirigin
  , packStatements
  , unpackStatement, unpackStatements
  , clusterifyStatements
  ) where

import YACP.Core.MyPrelude

import YACP.Core.Model.Identifier
import YACP.Core.Model.Relation

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

class (A.ToJSON a, A.FromJSON a, IdentifierProvider a, Eq a, Show a, Typeable a) => StatementContental a where
  getTypeRep :: a -> TypeRep
  getTypeRep = typeOf
  getDynamic :: a -> Dynamic
  getDynamic = toDyn
  getRelationFuns :: a -> [Identifier -> Relation]
  getRelationFuns _ = []

data Statement 
  = forall a. StatementContental a => Statement StatementMetadata a
instance Eq Statement where
  s1@(Statement sm1 sc1) == s2@(Statement sm2 sc2) = sm1 == sm2 && (show sc1 == show sc2)

instance Show Statement where
  show s@(Statement sm _) = show sm

instance A.ToJSON Statement where
  toJSON (Statement sm a) = A.object ["StatementMetadata" A..= sm
                                     , "StatementContent" A..= a
                                     , "StatementContentType" A..= (show $ getTypeRep a)
                                     ]

-- instance A.FromJSON Statement where
--   parseJSON = undefined

unpackStatement :: forall a. Typeable a => Statement -> Maybe a
unpackStatement (Statement _ x) = (fromDynamic . getDynamic) x

setOirigin1 :: Origin -> Statement -> Statement
setOirigin1 o (Statement (StatementMetadata i _) sc) = Statement (StatementMetadata i (Just o)) sc

instance IdentifierProvider Statement where
  getIdentifiers s@(Statement sm sc) = let
      identifiersFromMetatada = getIdentifiers sm
      identifiersFromContent = getIdentifiers sc
      identifiersFromRelations = getIdentifiers (getStatementRelations s)
    in identifiersFromMetatada <> identifiersFromContent <> identifiersFromRelations

instance Statemental Statement where
    getStatementMetadata (Statement sm _) = sm

    getStatementRelations' (Statement (StatementMetadata i _) c) = let
        funs = getRelationFuns c
      in Relations . V.fromList $ map (\fun -> fun i) funs

data Statements
  = Statements (Vector Statement)
  deriving (Generic)
deriving instance Eq Statements
deriving instance Show Statements
instance A.ToJSON Statements
-- instance A.FromJSON Statements
instance Semigroup Statements where
    (Statements rs1) <> (Statements rs2) = Statements (vNub (rs1 <> rs2))
instance Monoid Statements where
    mempty = Statements mempty
instance IdentifierProvider Statements where
  getIdentifiers (Statements ss) = V.concatMap getIdentifiers ss

packStatements :: StatementContental a => StatementMetadata -> [a] -> Statements
packStatements sm scs = Statements . V.fromList $ map (Statement sm) scs

unpackStatements :: forall a. Typeable a => Statements -> [a]
unpackStatements (Statements ss) = (catMaybes . map unpackStatement) (V.toList ss)

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