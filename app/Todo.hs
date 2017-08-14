{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Control.Lens
import Linear

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

fadeout :: String -> Auto (Chatter LMB) (Transit Str)
fadeout str = Auto
  { autoWatch = Down LMB
  , autoView = Transit 8 $ \t -> Str (V4 1 1 1 (1 - t)) str
  , autoUpdate = const transitIn
  }

main :: IO ()
main = start $ (,) (fillRGBA 0.14 0.19 0.22 1) $ Auto
  { autoWatch = (Always, Down KeyEnter)
  , autoView = VRec
    $ #box @:> Sized 0.1 (fillRGBA 0.5 0.5 0.55 1, TextBox)
    <: #list @:> Unsized (Rows :: Rows (Auto Hover (Transit Fill), Auto (Chatter LMB) (Transit Str)))
    <: nil
  , autoUpdate = \e s -> case e of
    Left _ -> s & #list %~ filter (\x -> x ^. _2 . autoState . _2 . _1 /= TRight)
    Right _ -> case s ^. #box . _2 . to textBoxText of
      "" -> s
      str -> s
        & #list %~ insertRows (hover 0.22 0.28 0.35, fadeout str)
        & #box . _2 %~ clearTextBox
  }
