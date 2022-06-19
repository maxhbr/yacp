{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module YACP.MyPrelude
    ( module X
    , tShow
    , vNub
    ) where

import           Prelude as X
import           GHC.Generics as X
import           Control.Applicative as X
import           Control.Monad as X
import           Data.Text as X (Text, pack)
import           Data.Vector as X (Vector)
import           Debug.Trace as X (trace)
import           System.Directory as X
import           System.FilePath as X
import           System.IO as X (hPutStrLn, stderr)
import Data.List as X (nub)

import qualified Data.Vector as V


tShow :: (Show a) => a -> Text
tShow = pack . show

vNub :: (Eq a) => V.Vector a -> V.Vector a
vNub = V.fromList . nub . V.toList
