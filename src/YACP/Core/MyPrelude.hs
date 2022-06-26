{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module YACP.Core.MyPrelude
  ( module X
  , tShow
  , vNub
  ) where

import           Control.Applicative           as X
import           Control.Monad                 as X
import           Data.List                     as X
                                                ( nub )
import           Data.Maybe                    as X
                                                ( fromMaybe
                                                , mapMaybe
                                                , isJust
                                                )
import           Data.String                   as X
                                                ( fromString )
import           Data.Text                     as X
                                                ( Text
                                                , pack
                                                )
import           Data.Vector                   as X
                                                ( Vector )
import           Debug.Trace                   as X
                                                ( trace )
import           GHC.Generics                  as X
import           Prelude                       as X
import           SPDX.Document.Common          as X
                                                ( SPDXMaybe(..) )
import           SPDX.LicenseExpression        as X
import           System.Directory              as X
import           System.FilePath               as X
import           System.IO                     as X
                                                ( hPutStrLn
                                                , stderr
                                                )

import Data.Monoid as X

import qualified Data.Vector                   as V


tShow :: (Show a) => a -> Text
tShow = pack . show

vNub :: (Eq a) => V.Vector a -> V.Vector a
vNub = V.fromList . nub . V.toList
