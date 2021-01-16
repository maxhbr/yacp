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
-- import           Data.Aeson as X
-- import           Data.ByteString.Lazy as X (ByteString)
-- import           Data.HashMap.Lazy as X (HashMap)
-- import           Data.List as X
-- import           Data.Map as X (Map)
-- import           Data.Maybe as X
-- import           Data.Monoid as X ((<>), mempty, mconcat)
-- import           Data.Text as T hiding (map)
import           Data.Text as X (Text, pack)
import           Data.Vector as X (Vector)
import           Debug.Trace as X (trace)
import           System.Directory as X
import           System.FilePath as X
import           System.IO as X (hPutStrLn, stderr)

tShow :: (Show a) => a -> Text
tShow = pack . show
