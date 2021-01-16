{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module YACP.Model
  ( module X
  ) where

import YACP.MyPrelude

import YACP.Model.Identifier as X
import YACP.Model.Component as X
import YACP.Model.Relation as X
