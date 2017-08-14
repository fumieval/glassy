module Glassy.Color where
import Linear

type RGBA = V4 Float

white :: RGBA
white = pure 1

fromHSV :: V3 Float -> V3 Float
fromHSV (V3 h_ s v) = pure (v - c) + rgb h_ where
  c = s * v
  rgb h
    | h < 0 = rgb (h + 360)
    | h < 60 = V3 c x 0
    | h < 120 = V3 x c 0
    | h < 180 = V3 0 c x
    | h < 240 = V3 0 x c
    | h < 300 = V3 x 0 c
    | h < 360 = V3 c 0 x
    | otherwise = rgb (h - 360)
  x = c * (1 - abs (h_ / 60 - fromIntegral (floor (h_ / 120) :: Int) * 2 - 1))

toHSV :: V3 Float -> V3 Float
toHSV (V3 r g b) = V3 h (s / maxC) maxC where
  maxC = r `max` g `max` b
  minC = r `min` g `min` b
  s = maxC - minC
  h | maxC == r = (g - b) / s * 60
    | maxC == g = (b - r) / s * 60 + 120
    | maxC == b = (r - g) / s * 60 + 240
    | otherwise = 0
