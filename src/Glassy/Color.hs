module Glassy.Color where
import Linear

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
