{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Model.Identifier
  ( Identifier (..)
  , matchesIdentifier
  , mkUUID
  , parsePURL
  , Identifiable (..)
  ) where

import YACP.MyPrelude

import Data.UUID (UUID)
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.List.Split (splitOn)
import System.Random (randomIO)
import qualified Network.URI as URI
import qualified System.FilePath as FP

{-|
  Class for Identifier
-}
data Identifier
  = Identifier String
  | UuidIdentifier UUID
  | PathIdentifier FilePath -- path of file
  | PURL (Maybe String) -- scheme
         (Maybe String) -- type
         (Maybe String) -- namespace
         String         -- name
         (Maybe String) -- version
         (Maybe String) -- qualifiers
         (Maybe String) -- subpath
  | Hash (Maybe String) -- type
         String         -- hash
  | Identifiers [Identifier] -- the best one is the head
  deriving (Eq)
instance Show Identifier where
  show (Identifier str) = str
  show (UuidIdentifier uuid) = show uuid
  show (PURL pScheme
            pType
            pNamespace
            pName
            pVersion
            pQualifier
            pSubpath) = let
    uri = URI.URI
          { URI.uriScheme = ("pkg" `fromMaybe` pScheme) ++ ":"
          , URI.uriAuthority = Nothing
          , URI.uriPath = FP.joinPath
            ( ([] `fromMaybe` (fmap (:[]) pType))
              ++ ([] `fromMaybe` (fmap (:[]) pNamespace))
              ++ [pName ++ (""`fromMaybe` (fmap ('@':) pVersion))]
            )
          , URI.uriQuery = "" `fromMaybe` pQualifier
          , URI.uriFragment = "" `fromMaybe` pSubpath
          }
    in show uri
  show (PathIdentifier fp) = fp
  show (Hash (Just t) h) = t ++ ":" ++ h
  show (Hash Nothing h) = h
  show (Identifiers is) = show is
flattenIdentifierToList :: Identifier -> [Identifier]
flattenIdentifierToList (Identifiers is) = nub $ concatMap flattenIdentifierToList is
flattenIdentifierToList i                = [i]

mkUUID :: IO Identifier
mkUUID = do
  uuid <- randomIO
  return (UuidIdentifier uuid)

parsePURL :: String -> Identifier
parsePURL uriStr = case URI.parseURI uriStr of
  Just uri -> let
      pScheme = Just (filter (/= ':') (URI.uriScheme uri))
      (pType, pNamespace, (pName, pVersion)) = let
          parseNameAndVersion :: String -> (String, Maybe String)
          parseNameAndVersion pNameAndVersion = case splitOn "@" pNameAndVersion of
            [pName, pVersion] -> (pName, Just pVersion)
            [pName]           -> (pName, Nothing)
            _                 -> (pNameAndVersion, Nothing)
          path = URI.uriPath uri
        in case FP.splitPath path of
          [pNameAndVersion] -> (Nothing, Nothing, parseNameAndVersion pNameAndVersion)
          [pType, pNameAndVersion] -> (Just (FP.dropTrailingPathSeparator pType), Nothing, parseNameAndVersion pNameAndVersion)
          (pType : ps) -> let
            pNameAndVersion = last ps
            pNamespace = (FP.dropTrailingPathSeparator . FP.joinPath . init) ps
            in (Just (FP.dropTrailingPathSeparator pType), Just pNamespace, parseNameAndVersion pNameAndVersion)
      pQualifier = case URI.uriQuery uri of
        [] -> Nothing
        qs -> Just $ qs
      pSubpath = case (URI.uriFragment uri) of
        "" -> Nothing
        fragment -> Just fragment
    in PURL pScheme
            pType
            pNamespace
            pName
            pVersion
            pQualifier
            pSubpath
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
