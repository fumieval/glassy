module Glassy.Transitive where
import Control.Applicative
import Linear
import Data.BoundingBox as Box

transits :: Transitive a => a -> a -> Float -> a
transits a b t = transit t a b

class Transitive a where
  -- | Smoothly transit (often linearly interpolate) between two values.
  --
  -- @
  -- transit 0 a b = a
  -- transit 1 a b = b
  -- @
  transit :: Float -> a -> a -> a

instance Transitive Float where
  transit t a b = (1 - t) * a + t * b

instance Transitive a => Transitive (V2 a) where
  transit = liftA2 . transit

instance Transitive a => Transitive (V3 a) where
  transit = liftA2 . transit

instance Transitive a => Transitive (V4 a) where
  transit = liftA2 . transit

instance (Transitive a, Transitive b) => Transitive (a, b) where
  transit t (a, b) (c, d) = (transit t a c, transit t b d)

instance (Applicative f, Transitive a) => Transitive (Box f a) where
  transit t (Box a b) (Box c d)
    = Box (transit t <$> a <*> c) (transit t <$> b <*> d)
