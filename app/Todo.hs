{-# LANGUAGE OverloadedLabels, OverloadedStrings, LambdaCase #-}
module Main where

import Glassy as G
import Data.Extensible
import Control.Lens
import Linear

hover :: Float -> Float -> Float -> Auto Hover (Transit Fill)
hover r g b = Auto
  { autoWatch = Hover -- when the cursor gets in or out of the area
  , autoUpdate = \case -- switch an animation
    True -> transitIn
    False -> transitOut
  , autoView = Transit 5 $ fillRGBA r g b
  }

fadeout :: String -> Auto (Chatter LMB) (Transit Str)
fadeout str = Auto
  { autoWatch = Down LMB -- when it's clicked
  , autoUpdate = const transitIn -- start an animation
  , autoView = Transit 8 $ \t -> Str (V4 1 1 1 (1 - t)) str
  }

main :: IO ()
main = start defaultGlassyConfig $ (,) (fillRGBA 0.14 0.19 0.22 1) $ Auto
  { autoWatch = (Always, Down KeyEnter)
  , autoUpdate = \e s -> case e of
    -- Always filter the list
    Left _ -> s & #list %~ filter (\x -> x
      ^. elemState
      . _2 -- the second element is `fadeout`
      . autoState
      . transitState /= TEnd)
    -- When Enter is pressed
    Right _ -> case s ^. #box . _2 . to textBoxText of
      "" -> s
      str -> s
        & #list %~ insertElem (hover 0.22 0.28 0.35, fadeout str)
        & #box . _2 %~ clearTextBox
  , autoView = VRec
    $ #box @:> Sized 0.1 (fillRGBA 0.3 0.3 0.35 1, TextBox)
    <: #list @:> Unsized (Rows :: Rows
      (Auto Hover (Transit Fill), Auto (Chatter LMB) (Transit Str)))
    <: nil
  }
