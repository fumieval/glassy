{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Linear
import Control.Lens

main :: IO ()
main = start $ VRec
  $ #counter @= Auto
      { autoInitial = 0 :: Int
      , autoWatch = LMB
      , autoView = Show
      , autoUpdate = \e -> _1 %~ case e of
        True -> (+1)
        _ -> id
      }
  <: #hello @= (Auto
      { autoInitial = ()
      , autoWatch = Hover
      , autoView = const $ Transit 5
        (Fill $ V4 0.0 0.0 0.0 1)
        (Fill $ V4 0.0 0.44 0.41 1)
      , autoUpdate = \e -> _2 %~ case e of
        True -> transitIn
        False -> transitOut
      }
    , HRec $ #h @= Str "Hello,"
      <: #w @= Str "World" <: nil)
  <: nil
