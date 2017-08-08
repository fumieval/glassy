{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
module Main where

import Glassy as G
import Data.Extensible

main :: IO ()
main = start $ VRec
  $ #counter @= Auto
      { autoInitial = 0 :: Int
      , autoView = \x -> (LMB, Show x)
      , autoUpdate = \_any -> (+1)
      }
  <: #hello @= (Fill $ rgb 0.6 0.44 0.41
    , HRec $ #h @= Str "Hello,"
      <: #w @= Str "World" <: nil)
  <: nil
