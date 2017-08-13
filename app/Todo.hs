{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Control.Lens
import Linear

main :: IO ()
main = start $ Auto
  { autoWatch = Down KeyEnter
  , autoView = VRec
    $ #box @:> Sized 0.1 (fillRGBA 0.1 0.2 0.15 1, TextBox)
    <: #list @:> Unsized (Self $ Rows
      $ map Str ["Turn my swag on"])
    <: nil
  , autoUpdate = \_ s -> case s ^. #box . _2 . textBoxText of
    "" -> s
    str -> s
      & #list . self %~ (\(Rows xs) -> Rows $ Str str : xs)
      & #box . _2 . textBoxText .~ ""
  }
