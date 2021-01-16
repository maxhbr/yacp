{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module YACP
  ( module X
  , someFunc
  ) where

import YACP.MyPrelude

import YACP.Model as X

someFunc :: IO ()
someFunc = putStrLn "someFunc"
