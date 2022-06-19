{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Model.Identifier
  ( Identifier (..)
  , matchesIdentifier
  , flattenIdentifierToList
  , mkUUID
  , parsePURL
  , Identifiable (..)
  ) where

import YACP.MyPrelude
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
  show (PURL purl) = show purl
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
