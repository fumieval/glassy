{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Linear
import Control.Lens

main :: IO ()
main = start $ VRec
  $ #counter @= Auto
    { autoWatch = Down LMB
    , autoView = Self (Show 0)
    , autoUpdate = const $ self %~ (+1)
    }
  <: #hello @= (Auto
    { autoWatch = Hover
    , autoView = Transit 5
      (Fill $ V4 0.0 0.0 0.0 1)
      (Fill $ V4 0.0 0.44 0.41 1)
    , autoUpdate = \case
      True -> transitIn
      False -> transitOut
    }
    , HRec $ #h @= Str "Hello,"
      <: #w @= Str "World" <: nil)
  <: nil
