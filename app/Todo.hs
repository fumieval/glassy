{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Control.Lens
import Linear

hover :: Float -> Float -> Float -> Auto Hover (Transit Fill)
hover r g b = Auto
  { autoWatch = Hover
  , autoView = Transit 5
    (fillRGBA r g b 0)
    (fillRGBA r g b 1)
  , autoUpdate = \case
    True -> transitIn
    False -> transitOut
  }

main :: IO ()
main = start $ (,) (fillRGBA 0.14 0.19 0.22 1) $ Auto
  { autoWatch = Down KeyEnter
  , autoView = VRec
    $ #box @:> Sized 0.1 (fillRGBA 0.5 0.5 0.55 1, TextBox)
    <: #list @:> Unsized (Rows :: Rows (Auto Hover (Transit Fill), Str))
    <: nil
  , autoUpdate = \_ s -> case s ^. #box . _2 . to textBoxText of
    "" -> s
    str -> s
      & #list %~ insertRows (hover 0.22 0.28 0.35, Str str)
      & #box . _2 %~ clearTextBox
  }
