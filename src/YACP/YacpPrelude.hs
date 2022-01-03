{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module YACP.YacpPrelude
  ( module X
  , tShow
  , objectNoNulls
  ) where

import           Control.Applicative as X
import           Control.Monad       as X
import           Data.Text           as X (Text, pack)
import           Data.Vector         as X (Vector)
import           Debug.Trace         as X (trace)
import           GHC.Generics        as X
import           Prelude             as X
import           System.Directory    as X
import           System.FilePath     as X
import           System.IO           as X (hPutStrLn, stderr)

import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as A

tShow :: (Show a) => a -> Text
tShow = pack . show

objectNoNulls :: [A.Pair] -> A.Value
objectNoNulls =
  let dropNulls []               = []
      dropNulls ((_, A.Null):ps) = dropNulls ps
      dropNulls (p:ps)           = p : (dropNulls ps)
   in A.object . dropNulls
