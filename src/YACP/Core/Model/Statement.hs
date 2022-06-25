
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}
module YACP.Core.Model.Statement
  ( Origin(..)
  , Statemental(..)
  , Statement(..)
  , setOrigin1
  , Statements(..)
  , getOrigins
  , setOrigin
  , packStatements
  , unpackStatement
  , unpackStatements
  , clusterifyStatements
  ) where

import           YACP.Core.MyPrelude

import           YACP.Core.Model.Identifier
import           YACP.Core.Model.Relation

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import           Data.Dynamic
import           Data.List                      ( nub )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , maybeToList
                                                )
import qualified Data.Monoid                    ( mconcat )
import           Data.String                    ( IsString(..) )
import qualified Data.Text                     as T
import           Data.Typeable
import           Data.UUID                      ( UUID )
import qualified Data.Vector                   as V
import qualified Distribution.Parsec           as SPDX
import qualified Distribution.SPDX             as SPDX
import qualified Distribution.SPDX.Extra       as SPDX
import qualified Distribution.SPDX.License     as SPDX
import qualified Network.URI                   as URI
import           System.Console.Pretty          ( Color(Green)
                                                , color
                                                )
import qualified System.FilePath               as FP
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.Random                  ( randomIO )

data Origin
  = NoOrigin
  | OriginTool String
  | OriginToolReport String FilePath
  deriving (Eq, Generic)
instance A.ToJSON Origin
instance A.FromJSON Origin
instance Show Origin where
  show (OriginTool name           ) = name
  show (OriginToolReport name file) = name ++ "@" ++ file

class (Eq a, Show a
      , Typeable a
      , A.ToJSON a {- , A.FromJSON a -}
      , IdentifierProvider a
      ) => Statemental a where
  getTypeRep :: a -> TypeRep
  getTypeRep = typeOf
  getDynamic :: a -> Dynamic
  getDynamic = toDyn
  getRelationFuns :: a -> [Identifier -> Relation]
  getRelationFuns _ = []

data Statement
  = forall a. Statemental a => Statement Identifier a
  | StatementWithOrigin Statement Origin
instance Eq Statement where
  (Statement i1 sc1) == (Statement i2 sc2) = i1 == i2 && (show sc1 == show sc2)
  (StatementWithOrigin s1 o1) == (StatementWithOrigin s2 o2) =
    s1 == s2 && o1 == o2
  _ == _ = False

instance Show Statement where
  show (Statement i sc) = show i ++ " -> " ++ show sc 
  show (StatementWithOrigin s _) = show s

instance A.ToJSON Statement where
  toJSON (Statement identifier a) = A.object
    [ "StatementSubject" A..= identifier
    , "StatementContent" A..= a
    , "StatementContentType" A..= (show $ getTypeRep a)
    ]
  toJSON (StatementWithOrigin s o) =
    A.object ["Statement" A..= s, "Origin" A..= o]

-- instance A.FromJSON Statement where
--   parseJSON = undefined

getStatementSubject :: Statement -> Identifier
getStatementSubject (Statement           i _) = i
getStatementSubject (StatementWithOrigin s _) = getStatementSubject s

unpackStatement :: forall a . Typeable a => Statement -> Maybe (a, Origin)
unpackStatement (Statement           _ x) = case (fromDynamic . getDynamic) x of
  Just y -> Just (y, NoOrigin)
  Nothing -> Nothing
unpackStatement (StatementWithOrigin s NoOrigin) = unpackStatement s
unpackStatement (StatementWithOrigin s o) = case unpackStatement s of
  Just (y,_) -> Just (y,o)
  Nothing -> Nothing

getOrigin :: Statement -> Origin
getOrigin (Statement _ _) = NoOrigin
getOrigin (StatementWithOrigin s NoOrigin) = getOrigin s
getOrigin (StatementWithOrigin _ o) = o

setOrigin1 :: Origin -> Statement -> Statement
setOrigin1 NoOrigin s = s
setOrigin1 o s = StatementWithOrigin s o

instance IdentifierProvider Statement where
  getIdentifiers (StatementWithOrigin s o) = getIdentifiers s
  getIdentifiers s@(Statement i sc) =
    let identifiersFromContent   = getIdentifiers sc
        identifiersFromRelations = getIdentifiers (getRelations s)
    in  i `V.cons` identifiersFromContent <> identifiersFromRelations

instance RelationProvider Statement where
  getRelations' (StatementWithOrigin s _) = getRelations' s
  getRelations' (Statement i c) =
    let funs = getRelationFuns c
    in  Relations . V.fromList $ map (\fun -> fun i) funs

data Statements = Statements (Vector Statement)
  deriving Generic
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

packStatements :: Statemental a => Identifier -> [a] -> Statements
packStatements i scs = Statements . V.fromList $ map (Statement i) scs

unpackStatements :: forall a . Typeable a => Statements -> [(a,Origin)]
unpackStatements (Statements ss) =
  (catMaybes . map unpackStatement) (V.toList ss)

getOrigins :: Statements -> [Origin]
getOrigins (Statements ss) = (nub . V.toList . V.map getOrigin) ss

setOrigin :: Origin -> Statements -> Statements
setOrigin o (Statements ss) = Statements (V.map (setOrigin1 o) ss)

clusterifyStatements :: Statements -> V.Vector (Identifier, Statements)
clusterifyStatements (ss@(Statements ss')) =
  let iClusters = getIdentifierClusters ss
      fun
        :: Vector (Identifier, Statements)
        -> Identifier
        -> Vector (Identifier, Statements)
      fun prev i =
        let matchingStatements =
              V.filter (\s -> getStatementSubject s `matchesIdentifier` i) ss'
        in  if V.null matchingStatements
              then prev
              else (i, Statements matchingStatements) `V.cons` prev
  in  V.foldl' fun V.empty iClusters
