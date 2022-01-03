{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeFamilies              #-}

module YACP.Model.Component
  ( Component(..)
  , identifierToComponent
  ) where

import           YACP.Model.Identifier
import           YACP.Model.Licenseable
import           YACP.Model.Relation
import           YACP.YacpPrelude

import qualified PURL.PURL               as PURL

import qualified Control.Monad.State     as MTL
import qualified Data.Aeson              as A
import qualified Data.Aeson.Types        as A
import           Data.List               (nub)
import           Data.List.Split         (splitOn)
import           Data.Maybe              (fromMaybe, maybeToList)
import qualified Data.Monoid             (mconcat)
import           Data.String             (IsString (..))
import qualified Data.Text               as T
import           Data.UUID               (UUID)
import qualified Data.Vector             as V
import qualified Distribution.Parsec     as SPDX
import qualified Distribution.SPDX       as SPDX
import qualified Distribution.SPDX.Extra as SPDX
import qualified SPDX.Document           as SPDX
import           System.Console.Pretty   (Color (Green), color)
import qualified System.FilePath         as FP
import           System.IO               (hPutStrLn, stderr)
import           System.Random           (randomIO)

--------------------------------------------------------------------------------
{-|
  Class for Component
-}
data Component =
  Component
    { _getComponentIdentifier    :: Identifier
    , _getComponentLicense       :: SPDX.MaybeLicenseExpression
    , _getComponentPayload       :: A.Array
    , _getComponentRelations     :: [Relation]
    , _getComponentSubComponents :: [Component]
    }
  deriving (Eq, Generic)

instance A.ToJSON Component

instance A.FromJSON Component

instance Show Component where
  show (Component {_getComponentIdentifier = cId, _getComponentLicense = l}) =
    "{{{" ++ show cId ++ "@" ++ show l ++ "}}}"

instance Licenseable Component where
  getLicense = _getComponentLicense

instance Identifiable Component where
  getIdentifier = _getComponentIdentifier
  addIdentifier (c@Component {_getComponentIdentifier = is}) i =
    c {_getComponentIdentifier = is <> i}

instance Semigroup Component where
  c1 <> c2 =
    let mergedIdentifiers = (getIdentifier c1) <> (getIdentifier c2)
        mergedLicense =
          SPDX.MLicExp $
          let l1 = getRawLicense c1
              l2 = getRawLicense c2
           in case l1 of
                SPDX.SPDXJust l1' ->
                  case l2 of
                    SPDX.SPDXJust l2' -> SPDX.SPDXJust (l1' `SPDX.EOr` l2')
                    _                 -> l1
                _ -> l2
        mergedPayload =
          let p1 = _getComponentPayload c1
              p2 = _getComponentPayload c2
           in if p1 /= p2
                then p1 <> p2
                else p1
        mergedRelations =
          let r1 = _getComponentRelations c1
              r2 = _getComponentRelations c2
           in nub (r1 ++ r2)
        mergedSubComponents =
          let sc1 = _getComponentSubComponents c1
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
  mempty = Component mempty mempty mempty mempty mempty

identifierToComponent :: Identifier -> Component
identifierToComponent i = mempty {_getComponentIdentifier = i}
