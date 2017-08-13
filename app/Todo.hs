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
    $ #box @:> Sized 0.1 (Fill (V4 120 0.3 0.3 1)
      , TextBox)
    <: #list @:> Unsized (Self $ Rows
      $ map Str ["Turn my swag on"])
    <: nil
  , autoUpdate = \_ s -> let str = s ^. #box . _2 . textBoxText
      in s
        & #list . self %~ (\(Rows xs) -> Rows $ Str str : xs)
        & #box . _2 .~ Right ("", 0)
  }
