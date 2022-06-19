{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module YACP.Model.License
  ( module SPDX
  , Licenseable (..)
  ) where

import YACP.MyPrelude
import  SPDX.LicenseExpression as SPDX

class Licenseable a where
  getLicense :: a -> SPDX.MaybeLicenseExpression
  showLicense :: a -> String
  showLicense = show . getLicense
