{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Linear

main :: IO ()
main = start $ VRec
  $ #counter @= Auto
      { autoInitial = 0 :: Int
      , autoWatch = LMB
      , autoView = Show
      , autoUpdate = \case
        True -> (+1)
        _ -> id
      , autoOverride = const id
      }
  <: #hello @= (Auto
      { autoInitial = ()
      , autoWatch = Hover
      , autoView = const $ Transit 5
        (Fill $ V4 0.0 0.0 0.0 1)
        (Fill $ V4 0.0 0.44 0.41 1)
      , autoUpdate = const id
      , autoOverride = \case
        True -> transitIn
        False -> transitOut
      }
    , HRec $ #h @= Str "Hello,"
      <: #w @= Str "World" <: nil)
  <: nil
