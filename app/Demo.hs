{-# LANGUAGE OverloadedLabels #-}
module Main where

import qualified Glassy as G
import Data.Extensible

main :: IO ()
main = G.start $ G.VRec
  $ #counter @= G.Auto 0 (\x -> (G.LMB, G.Show x)) (const (+1))
  <: #hello @= "Hello, world"
  <: nil
