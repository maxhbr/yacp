{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Model
  ( module X
  ) where

import YACP.MyPrelude as X
import YACP.Model.Identifier as X
import YACP.Model.Origin as X
import YACP.Model.Relation as X
import YACP.Model.License as X
import YACP.Model.Component as X
import YACP.Model.File as X