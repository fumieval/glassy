{-# LANGUAGE TypeFamilies, DataKinds, FlexibleContexts, TemplateHaskell #-}
module Main where

import qualified Glassy as G
import Data.Extensible

mkField "Counter Hello"

main :: IO ()
main = G.start $ G.VRec
  $ _Counter @= G.Auto 0 (\x -> (G.LMB, G.Show x)) (const (+1))
  <: _Hello @= "Hello, world"
  <: nil
