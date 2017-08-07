{-# LANGUAGE OverloadedLabels #-}
module Main where

import Glassy as G
import Data.Extensible

main :: IO ()
main = start $ RowRec
  $ #counter @= Auto
      { autoInitial = 0 :: Int
      , autoView = \x -> (LMB, Show x)
      , autoUpdate = const (+1)
      }
  <: #hello @= Str "Hello, world"
  <: nil
