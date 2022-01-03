{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeFamilies              #-}

module YACP.Model
  ( module X
  -- Actions
  , addRoot
  , addRoots
  , addComponent
  , addComponents
  , addRelation
  , addRelations
  , addFile
  , addFiles
  , getForIdentifier
  , getCsForIdentifier
  , getRsForIdentifier
  , addYACPIssue
  -- misc
  , stderrLog
  ) where

import           YACP.Model.Component            as X
import           YACP.Model.File                 as X
import           YACP.Model.Identifier           as X
import           YACP.Model.Relation             as X
import           YACP.Model.State                as X
import           YACP.YacpPrelude

import qualified PURL.PURL                       as PURL
import           SPDX.Document.RelationshipTypes as X
import           YACP.YacpPrelude                as X

import qualified Control.Monad.State             as MTL
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Types                as A
import           Data.List                       (nub)
import           Data.List.Split                 (splitOn)
import           Data.Maybe                      (fromMaybe, maybeToList)
import qualified Data.Monoid                     (mconcat)
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import           Data.UUID                       (UUID)
import qualified Data.Vector                     as V
import qualified Distribution.Parsec             as SPDX
import qualified Distribution.SPDX               as SPDX
import qualified Distribution.SPDX.Extra         as SPDX
import qualified SPDX.Document                   as SPDX
import           System.Console.Pretty           (Color (Green), color)
import qualified System.FilePath                 as FP
import           System.IO                       (hPutStrLn, stderr)
import           System.Random                   (randomIO)

-------------------------------------------------------------------------------
stderrLog :: String -> YACP ()
stderrLog msg = MTL.liftIO $ hPutStrLn stderr (color Green msg)

addRoot :: Identifier -> YACP ()
addRoot r = MTL.modify (\s@State {_getRoots = rs} -> s {_getRoots = r : rs})

addRoots :: Vector Identifier -> YACP ()
addRoots = V.mapM_ addRoot

addComponent :: Component -> YACP Identifier
addComponent =
  let addComponent' :: Component -> Components -> Components
      addComponent' c (Components cs) =
        let identifier = getIdentifier c
            nonMatchingCs =
              V.filter (not . (identifier `matchesIdentifiable`)) cs
            matchingCs = V.filter (identifier `matchesIdentifiable`) cs
            mergedC = c <> ((mconcat . V.toList) matchingCs)
         in Components (mergedC `V.cons` nonMatchingCs)
   in \c -> do
        mapM_ addRelation (_getComponentRelations c)
        mapM_ addComponent (_getComponentSubComponents c)
        c' <- MTL.liftIO $ addUuidIfMissing c
        MTL.modify
          (\s@State {_getComponents = cs} ->
             s {_getComponents = c' `addComponent'` cs})
        c'' <-
          MTL.gets
            (\State {_getComponents = Components cs} ->
               case (V.find ((getIdentifier c') `matchesIdentifiable`) cs) of
                 Just c'' -> c''
                 Nothing  -> c')
        return (getIdentifier c'')

addComponents :: Vector Component -> YACP ()
addComponents = V.mapM_ addComponent

addRelation :: Relation -> YACP ()
addRelation =
  let addRelation' :: Relation -> Relations -> Relations
      addRelation' r (Relations rs) = Relations (r `V.cons` rs)
      addRelationEdgesToComponents :: Relation -> YACP Relation
      addRelationEdgesToComponents (r@(Relation src _ target)) = do
        src' <- addComponent (identifierToComponent src)
        target' <- addComponent (identifierToComponent target)
        return r {_getRelationSrc = src', _getRelationTarget = target'}
      flipDirection :: Relation -> Relation
      flipDirection (r@Relation { _getRelationSrc = src
                                , _getRelationTarget = target
                                }) =
        r {_getRelationSrc = target, _getRelationTarget = src}
      normalizeRelation :: Relation -> Relation
      normalizeRelation (r@Relation {_getRelationType = DEPENDS_ON}) =
        flipDirection (r {_getRelationType = DEPENDENCY_OF})
      normalizeRelation (r@Relation {_getRelationType = DESCRIBED_BY}) =
        flipDirection (r {_getRelationType = DESCRIBES})
      normalizeRelation (r@Relation {_getRelationType = CONTAINS}) =
        flipDirection (r {_getRelationType = CONTAINED_BY})
      normalizeRelation (r@Relation {_getRelationType = HAS_PREREQUISITE}) =
        flipDirection (r {_getRelationType = PREREQUISITE_FOR})
      normalizeRelation (r@Relation {_getRelationType = GENERATED_FROM}) =
        flipDirection (r {_getRelationType = GENERATES})
      normalizeRelation r = r
   in \r -> do
        r' <- addRelationEdgesToComponents (normalizeRelation r)
        MTL.modify
          (\s@State {_getRelations = rs} ->
             s {_getRelations = r' `addRelation'` rs})

addRelations :: Vector Relation -> YACP ()
addRelations = V.mapM_ addRelation

addFile :: File -> YACP ()
addFile f =
  MTL.modify
    (\s@State {_getFiles = Files fs} -> s {_getFiles = Files (f `V.cons` fs)})

addFiles :: Vector File -> YACP ()
addFiles = V.mapM_ addFile

getCsForIdentifier :: Identifier -> YACP Components
getCsForIdentifier identifier =
  MTL.get >>= \(State {_getComponents = Components cs}) ->
    return $ Components (V.filter (identifier `matchesIdentifiable`) cs)

getRsForIdentifier :: Identifier -> YACP Relations
getRsForIdentifier identifier =
  MTL.get >>= \(State {_getRelations = Relations rs}) ->
    return $ Relations (V.filter (identifier `relationContainsIdentifier`) rs)

getForIdentifier :: Identifier -> YACP State
getForIdentifier identifier = do
  csForIdentifier <- getCsForIdentifier identifier
  rsForIdentifier <- getRsForIdentifier identifier
  MTL.get >>= \(State { _getRoots = roots
                      , _getRelations = Relations rs
                      , _getFiles = Files fs
                      }) ->
    let rootsMatchingIdentifier = filter (identifier `matchesIdentifier`) roots
        filesMatchingIdentifier =
          Files (V.filter (identifier `matchesIdentifiable`) fs)
     in return
          (State
             rootsMatchingIdentifier
             csForIdentifier
             rsForIdentifier
             filesMatchingIdentifier
             [])

addYACPIssue :: YACPIssue -> YACP ()
addYACPIssue i = do
  case i of
    YACPParsingIssue err -> stderrLog ("Parsing failed with: " ++ err)
    YACPFileParsingIssue fp err ->
      stderrLog ("Parsing of " ++ fp ++ " failed with: " ++ err)
  MTL.modify (\s@State {_getYacpIssues = is} -> s {_getYacpIssues = i : is})
