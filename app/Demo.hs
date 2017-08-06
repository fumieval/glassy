{-# LANGUAGE OverloadedLabels #-}
module Main where

import Glassy as G
import Data.Extensible

main :: IO ()
main = start $ VRec
  $ #counter @= Auto
      { autoInitial = 0
      , autoView = \x -> (LMB, Show x)
      , autoUpdate = const (+1)
      }
  <: #hello @= "Hello, world"
  <: nil
