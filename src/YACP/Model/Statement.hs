{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Model.Statement
  ( Statement (..)
  ) where

import YACP.MyPrelude
import YACP.Model.Identifier

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

data Statement a
    = Statement
    { _getStatementOrigin :: Origin
    , _getStatementAffected :: Identifier
    , _getStatementContent :: a
    }
