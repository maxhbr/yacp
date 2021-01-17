{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module YACP.MyPrelude
    ( module X
    , tShow
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

tShow :: (Show a) => a -> Text
tShow = pack . show
