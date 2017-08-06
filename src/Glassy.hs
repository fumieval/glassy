{-# LANGUAGE BangPatterns, TypeFamilies, ScopedTypeVariables, Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
module Glassy where

import Graphics.Holz hiding (Chatter(..), draw)
import Graphics.Holz.Vertex hiding (draw)
import qualified Graphics.Holz.Text as Text
import Control.Monad
import Linear
import Data.Function
import Data.Monoid
import Data.Proxy
import Data.Profunctor
import Data.Void
import System.IO.Unsafe
import Data.Extensible hiding (State)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import GHC.TypeLits
import Control.Monad.IO.Class

class Glassy a where
  type State a
  type State a = ()
  type Event a
  type Event a = Void
  initialState :: a -> State a

  default initialState :: a -> ()
  initialState _ = ()

  draw :: a -> State a -> Box V2 Float -> ShaderT (HolzT IO) (State a)
  draw _ s _ = return s

  poll :: a -> State a -> Box V2 Float -> ShaderT (HolzT IO) (State a, [Event a])
  poll _ s _ = return (s, [])

theTypewriter :: Text.Renderer
theTypewriter = unsafePerformIO
  $ Text.typewriter "/System/Library/Fonts/ヒラギノ角ゴシック W9.ttc"

instance (c ~ Char) => Glassy [c] where
  draw str _ (Box (V2 x0 y0) (V2 x1 y1)) = Text.runRenderer theTypewriter $ do
    let size = (y1 - y0) * 2 / 3
    let fg = pure 1
    Text.string size fg str
    V2 x y <- Text.getOffset
    let k = min 1 $ (x1 - x0) / x
    Text.render $ translate (V3 (x1 - 4 - k * x) (y0 + (y1 - y0) * 0.75 - k * y) 1)
      !*! scaled (V4 k k k 1)
    Text.clear

newtype Show a = Show { getShow :: a }
  deriving (Bounded, Enum, Eq, Floating, Fractional, Integral, Monoid, Num, Ord
    , Real, RealFrac, RealFloat)

instance Prelude.Show a => Glassy (Glassy.Show a) where
  draw (Show a) = draw (show a)

instance Glassy a => Glassy (IO a) where
  type State (IO a) = Maybe (State a)
  type Event (IO a) = Event a
  initialState _ = Nothing
  draw m s box = do
    a <- liftIO m
    Just <$> draw a (maybe (initialState a) id s) box
  poll m s box = do
    a <- liftIO m
    (s', es) <- poll a (maybe (initialState a) id s) box
    return (Just s', es)

data LMB = LMB

instance Glassy LMB where
  type State LMB = Bool
  type Event LMB = ()
  initialState _ = False
  poll LMB b _ = do
    f <- mousePress 0
    return (f, guard (not b && f))

instance Glassy Key where
  type State Key = Bool
  type Event Key = ()
  initialState _ = False
  poll k b _ = do
    f <- keyPress k
    return (f, guard (not b && f))

data Auto s a = Auto
    { autoInitial :: s
    , autoView :: s -> a
    , autoUpdate :: Event a -> s -> s
    }

instance Glassy a => Glassy (Auto s a) where
  type State (Auto s a) = (s, State a)
  type Event (Auto s a) = Event a
  initialState (Auto s f _) = (s, initialState $ f s)
  draw (Auto _ f _) (s, t) box = (,) s <$> draw (f s) t box
  poll (Auto _ f u) (s, t) box = do
    (t', es) <- poll (f s) t box
    let !s' = foldr u s es
    return ((s', t'), es)

instance (Glassy a, Glassy b) => Glassy (a, b) where
  type State (a, b) = (State a, State b)
  type Event (a, b) = Either (Event a) (Event b)
  initialState (a, b) = (initialState a, initialState b)
  draw (a, b) (s, t) box = (,)
    <$> draw a s box
    <*> draw b t box

  poll (a, b) (s, t) box = do
    (s', es) <- poll a s box
    (t', fs) <- poll b t box
    return ((s', t'), map Left es ++ map Right fs)

start :: forall a. Glassy a => a -> IO ()
start a = withHolz $ do
  win <- openWindow Windowed $ Box (V2 0 0) (V2 640 480)
  sh <- makeShader
  void $ retract $ runHolzT win
    $ ($ initialState a)
    $ fix $ \self s -> (>>=self) $ withFrame win $ do
      pos <- getCursorPos
      runShaderT sh $ do
        setOrthographic
        box <- getBoundingBox
        (s', es) <- poll a s box
        draw a s' box

newtype VRec xs = VRec { getVRec :: RecordOf Sized xs }

data Sized a = Sized !Float !a | Unsized !a

instance Wrapper Sized where
  type Repr Sized a = a
  _Wrapper = dimap (\case
    Sized _ a -> a
    Unsized a -> a) (fmap Unsized)

newtype WrapState a = WrapState { unwrapState :: State a }

withSubbox :: (Monad m, Forall (KeyValue KnownSymbol Glassy) xs)
  => RecordOf Sized xs
  -> (forall x. Glassy (AssocValue x) => Membership xs x -> AssocValue x -> Box V2 Float -> m (h (AssocValue x)))
  -> Box V2 Float
  -> m (RecordOf h xs)
withSubbox rec k (Box (V2 x0 y0) (V2 x1 y1)) = flip evalStateT y0
  $ hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol Glassy))
  $ \i -> StateT $ \y ->
    let (h, a) = case getField $ hindex rec i of
              Sized f a -> (f * height, a)
              Unsized a -> (freeRatio * height, a)
    in do
      s' <- k i a $ Box (V2 x0 y) (V2 x1 (y + h))
      return (Field s', y + h)
  where
    height = y1 - y0
    freeRatio = (1 - reserve) / count
    (Sum reserve, Sum count) = hfoldMap (\c -> case getField c of
      Sized f _ -> (Sum f, Sum 0)
      Unsized a -> (mempty, Sum 1)) rec

instance Forall (KeyValue KnownSymbol Glassy) xs => Glassy (VRec xs) where
  type State (VRec xs) = RecordOf WrapState xs
  type Event (VRec xs) = Void
  initialState (VRec rec) = htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol Glassy))
    $ \i -> Field $ WrapState $ initialState $ case getField $ hindex rec i of
      Sized _ a -> a
      Unsized a -> a
  draw (VRec rec) states = withSubbox rec
    $ \i a -> fmap WrapState . draw a (unwrapState $ getField $ hindex states i)
  poll (VRec rec) states = (fmap (flip (,) []).)
    $ withSubbox rec $ \i a -> fmap (WrapState . fst) . poll a (unwrapState $ getField $ hindex states i)
