{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeFamilies              #-}

module YACP.State
  ( State(..)
  , YACP(..)
  , runYACP
  , runYACP'
  -- actions
  , stderrLog
  , addRoot
  , addStatement
  , addYACPIssue
  ) where

import YACP.MyPrelude
import           YACP.Model


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
import qualified SPDX.Document.Common            as SPDX
import           System.Console.Pretty           (Color (Green), color)
import qualified System.FilePath                 as FP
import           System.IO                       (hPutStrLn, stderr)
import           System.Random                   (randomIO)


data YACPIssue
  = YACPParsingIssue String
  | YACPFileParsingIssue FilePath String
  deriving (Eq, Show, Generic)

instance A.ToJSON YACPIssue

instance A.FromJSON YACPIssue

data State =
  State
    { _getRoots      :: [Identifier]
    , _getStatements :: Statements
    , _getYacpIssues :: [YACPIssue]
    }
  deriving (Eq, Show, Generic)

instance A.ToJSON State

instance A.FromJSON State

type YACP a = MTL.StateT State IO a

runYACP :: YACP a -> IO (a, State)
runYACP yacp =
  let initialState =
        State mempty mempty mempty
   in runYACP' yacp initialState

runYACP' :: YACP a -> State -> IO (a, State)
runYACP' yacp initialState = MTL.runStateT yacp initialState


addRoot :: Identifier -> YACP ()
addRoot r = MTL.modify (\s@State {_getRoots = rs} -> s {_getRoots = r : rs})

addStatement :: Statement -> YACP ()
addStatement = undefined

addYACPIssue :: YACPIssue -> YACP ()
addYACPIssue i = do
  case i of
    YACPParsingIssue err -> stderrLog ("Parsing failed with: " ++ err)
    YACPFileParsingIssue fp err ->
      stderrLog ("Parsing of " ++ fp ++ " failed with: " ++ err)
  MTL.modify (\s@State {_getYacpIssues = is} -> s {_getYacpIssues = i : is})

stderrLog :: String -> YACP ()
stderrLog msg = MTL.liftIO $ hPutStrLn stderr (color Green msg)