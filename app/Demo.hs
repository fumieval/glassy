{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Linear

main :: IO ()
main = start $ VRec
  $ #counter @= Auto
      { autoInitial = 0 :: Int
      , autoView = \x -> (LMB, Show x)
      , autoUpdate = \_any -> (+1)
      }
  <: #hello @= (Override
      { overrideTarget = (Hover, Transit 5
        (Fill $ V4 0.0 0.0 0.0 1)
        (Fill $ V4 0.0 0.44 0.41 1))
      , overrideState = \e (t, (s, ())) -> case e of
        Left True -> (t, (transitIn s, ()))
        Left False -> (t, (transitOut s, ()))
        _ -> (t, (s, ()))
      }
    , HRec $ #h @= Str "Hello,"
      <: #w @= Str "World" <: nil)
  <: nil
