{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Control.Lens
import Linear

main :: IO ()
main = start $ Auto
  { autoInitial = ["Turn my swag on"]
  , autoWatch = KeyEnter
  , autoView = \xs -> VRec
    $ #box @:> Sized 0.1 (Fill (V4 0 0.3 0.3 1), TextBox)
    <: #list @:> Unsized (Rows $ map Str xs)
    <: nil
  , autoUpdate = \case
    True -> \s -> case s ^? _2 . #box . _2 . _Right . _1 of
      Just str -> s & _1 %~ (str :)
        & _2 . #box . _2 .~ Right ("", 0)
      _ -> s
    _ -> id
  }
