{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Linear
import Control.Lens

hover :: Float -> Float -> Float -> Auto Hover (Transit Fill)
hover r g b = Auto
  { autoWatch = Hover
  , autoView = Transit 5 $ transits
    (fillRGBA r g b 0)
    (fillRGBA r g b 1)
  , autoUpdate = \case
    True -> transitIn
    False -> transitOut
  }

main :: IO ()
main = start $ VRec
  $ #counter @= Auto
    { autoWatch = Down LMB
    , autoView = (hover 0.4 0.52 0.33, Show 0)
    , autoUpdate = const $ _2 +~ 1
    }
  <: #hello @= (hover 0.5 0.3 0.2
    , HRec $ #h @= Str white "Hello,"
      <: #w @= Str white "World" <: nil)
  <: nil
