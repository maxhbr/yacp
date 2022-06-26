{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}
module YACP.Core.Model.Identifier
  ( module PURL
  -- Identifier
  , Identifier(..)
  , matchesIdentifier
  , flattenIdentifierToList
  , mkUUID
  , Identifiable(..)
  , clusterifyIdentifiers
  , IdentifierProvider(..)
  ) where

import qualified PURL.PURL                     as PURL
import           YACP.Core.MyPrelude

import qualified Control.Monad.State           as MTL
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import           Data.Char                      ( toLower )
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
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           System.Random                  ( randomIO )

--------------------------------------------------------------------------------
data Identifier
  = Identifier String
  | UuidIdentifier UUID
  | AbsolutePathIdentifier FilePath
  | RelativePathIdentifier Identifier FilePath
  | UrlIdentifier String
  | PurlIdentifier PURL.PURL
  | Hash (Maybe String) String
  | Identifiers [Identifier]
  deriving (Eq, Generic)
instance A.ToJSON Identifier
instance A.FromJSON Identifier
instance IsString Identifier where
  fromString = Identifier
instance Show Identifier where
  show (Identifier             str        ) = str
  show (UuidIdentifier         uuid       ) = show uuid
  show (UrlIdentifier          url        ) = url
  show (PurlIdentifier         purl       ) = show purl
  show (AbsolutePathIdentifier fp         ) = fp
  show (RelativePathIdentifier _        fp) = fp
  show (Hash                   (Just t) h ) = t ++ ":" ++ h
  show (Hash                   Nothing  h ) = h
  show (Identifiers [i]                   ) = show i
  show (Identifiers is                    ) = show is
flattenIdentifierToList :: Identifier -> [Identifier]
flattenIdentifierToList (Identifiers is) =
  nub $ concatMap flattenIdentifierToList is
flattenIdentifierToList i = [i]

mkUUID :: IO Identifier
mkUUID = do
  uuid <- randomIO
  return (UuidIdentifier uuid)

instance Semigroup Identifier where
  i1               <> (Identifiers []) = i1
  (Identifiers []) <> i2               = i2
  i1 <> i2 =
    let i1List = flattenIdentifierToList i1
        i2List = flattenIdentifierToList i2
    in  Identifiers (nub $ i1List ++ i2List)
instance Monoid Identifier where
  mempty = Identifiers []

matchesIdentifier :: Identifier -> Identifier -> Bool
matchesIdentifier i1 i2 =
  let
    matchesIdentifier' :: Identifier -> Identifier -> Bool
    matchesIdentifier' (Hash _ h1) (Hash _ h2) = h1 == h2 -- ignore type
    matchesIdentifier' (PurlIdentifier p1) (PurlIdentifier p2) =
      let
        toLowerString :: String -> String
        toLowerString = map toLower
        toLowerMaybe :: Maybe String -> Maybe String
        toLowerMaybe = fmap toLowerString
      in
        and
          [ PURL._PURL_type p1 == PURL._PURL_type p2
          , toLowerMaybe (PURL._PURL_namespace p1)
            == toLowerMaybe (PURL._PURL_namespace p2)
          , toLowerString (PURL._PURL_name p1)
            == toLowerString (PURL._PURL_name p2)
          , toLowerMaybe (PURL._PURL_version p1)
            == toLowerMaybe (PURL._PURL_version p2)
          ] -- ignore qualifiers and subpath
    matchesIdentifier' i1' i2' = i1' == i2' -- TODO: better matching? wildcards? path sanitation?

    i1s     = flattenIdentifierToList i1
    i2s     = flattenIdentifierToList i2
    product = [ (a, b) | a <- i1s, b <- i2s ]
  in
    any (uncurry matchesIdentifier') product

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

clusterifyIdentifiers :: Identifiers -> Identifiers
clusterifyIdentifiers input =
  let lenOld = V.length input
      fun :: Identifiers -> Identifier -> Identifiers
      fun acc i = case V.uncons acc of
        Just (i', acc') -> if i `matchesIdentifier` i'
          then (i <> i') `V.cons` acc'
          else i' `V.cons` (fun acc' i)
        Nothing -> V.singleton i
      output = V.foldl' fun V.empty input
      lenNew = V.length output
  in  if lenOld == lenNew then output else clusterifyIdentifiers output

type Identifiers = V.Vector Identifier
class IdentifierProvider a where
  getIdentifiers :: a -> Identifiers
  getIdentifiers = mempty
  getIdentifierClusters :: a -> Identifiers
  getIdentifierClusters = clusterifyIdentifiers . getIdentifiers
instance IdentifierProvider a => IdentifierProvider (Maybe a) where
  getIdentifiers (Just a) = getIdentifiers a
  getIdentifiers Nothing  = mempty
