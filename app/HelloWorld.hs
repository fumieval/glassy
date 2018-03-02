module Main where

import Glassy

main :: IO ()
main = start defaultGlassyConfig $ Str white "Hello, world"
