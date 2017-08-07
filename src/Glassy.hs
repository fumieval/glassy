{-# LANGUAGE BangPatterns, TypeFamilies, ScopedTypeVariables, Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, DefaultSignatures #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
module Glassy where

import Control.Concurrent (threadDelay)
import Control.Lens ((??))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Writer
import qualified Data.BoundingBox as Box
import Data.Extensible hiding (State)
import Data.Extensible.Effect.Default
import Data.Profunctor
import Data.Proxy
import Data.Time.Clock
import Data.Void
import GHC.TypeLits
import Graphics.Holz hiding (Chatter(..), draw)
import Linear
import qualified Graphics.Holz.Text as Text
import qualified System.Info as Info

type HolzEffs =
  [ "box" >: ReaderEff (Box V2 Float)
  , "holzShader" >: ReaderEff Shader
  , "holzWindow" >: ReaderEff Window
  , "font" >: ReaderEff Text.Renderer
  , "IO" >: IO]

type GlassyEffs a = StateDef (State a)
    ': ("event" >: WriterEff (Event a))
    ': HolzEffs

class Glassy a where
  type State a
  type State a = ()
  type Event a
  type Event a = Void
  initialState :: a -> State a

  default initialState :: a -> ()
  initialState _ = ()

  poll :: a -> Eff (GlassyEffs a) (Eff HolzEffs ())
  poll _ = return $ return ()

start :: forall a. Glassy a => a -> IO ()
start a = withHolz $ do
  win <- openWindow Windowed $ Box (V2 0 0) (V2 640 480)
  sh <- makeShader
  font <- case Info.os of
    "linux" -> Text.typewriter "/usr/share/fonts/truetype/takao-gothic/TakaoPGothic.ttf"
    os -> do
      print os
      Text.typewriter "/System/Library/Fonts/ヒラギノ角ゴシック W9.ttc"
  void $ retractEff @ "IO"
    $ runMaybeEff @ "close"
    $ runReaderEff @ "font" ?? font
    $ runReaderEff @ "holzWindow" ?? win
    $ execStateEff @ "State" ?? initialState a
    $ forever $ do
      t0 <- liftIO getCurrentTime
      withFrame win $ runReaderEff @ "holzShader" ?? sh $ do
        box <- liftHolz $ do
          setOrthographic
          getBoundingBox
        runReaderEff @ "box" ?? box $ do
          draw <- pipeWriterEff @ "event" (const $ return ()) $ castEff $ poll a
          castEff draw
      t1 <- liftIO getCurrentTime
      liftIO $ threadDelay $ floor $ (*1e6)
        $ 1 / 30 - (realToFrac (diffUTCTime t1 t0) :: Double)
      shouldClose <- runReaderT windowShouldClose win
      when shouldClose $ throwEff #close ()

liftHolz :: (Associate "holzShader" (ReaderEff Shader) xs
  , Associate "holzWindow" (ReaderEff Window) xs
  , Associate "IO" IO xs)
  => ShaderT (ReaderT Window IO) a -> Eff xs a
liftHolz m = do
  sh <- askEff #holzShader
  win <- askEff #holzWindow
  liftIO $ runShaderT sh m `runReaderT` win

newtype Str = Str String

instance Glassy Str where
  poll (Str str) = return $ do
    Box (V2 x0 y0) (V2 x1 y1) <- askEff #box
    font <- askEff #font
    liftHolz $ Text.runRenderer font $ do
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
  poll (Show a) = poll $ Str $ show a

instance Glassy a => Glassy (Eff HolzEffs a) where
  type State (Eff HolzEffs a) = Maybe (State a)
  type Event (Eff HolzEffs a) = Event a
  initialState _ = Nothing
  poll m = do
    a <- castEff m
    s <- maybe (initialState a) id <$> get
    (draw, s') <- castEff $ runStateDef (poll a) s
    put $ Just s'
    return draw

-- | A moore machine that handles events from the output
data Auto s a = Auto
  { autoInitial :: s
  , autoView :: s -> a
  , autoUpdate :: Event a -> s -> s
  }

pipeWriterEff :: forall k w xs a. (w -> Eff xs ())
  -> Eff (k >: WriterEff w ': xs) a
  -> Eff xs a
pipeWriterEff p = peelEff0 return $ \(w, a) k -> p w >> k a

enumWriterEff :: forall k w xs a. Eff (k >: WriterEff w ': xs) a
  -> Eff xs (a, [w])
enumWriterEff = peelEff1 (\a k -> return (a, k [])) (\(w, a) k f -> k a $ (w:) . f)
  `flip` id

instance Glassy a => Glassy (Auto s a) where
  type State (Auto s a) = (s, State a)
  type Event (Auto s a) = Event a
  initialState (Auto s f _) = (s, initialState $ f s)
  poll (Auto _ f u) = do
    (s, t) <- get
    ((draw, t'), es) <- castEff $ enumWriterEff $ poll (f s) `runStateDef` t
    let !s' = foldr u s es
    put (s', t')
    return draw

newtype RowRec (xs :: [Assoc Symbol *]) = RowRec { getRowRec :: RecordOf Sized xs }

data Sized a = Sized !Float !a | Unsized !a

instance Wrapper Sized where
  type Repr Sized a = a
  _Wrapper = dimap (\case
    Sized _ a -> a
    Unsized a -> a) (fmap Unsized)

newtype WrapState a = WrapState { unwrapState :: State a }

withSubbox :: (Monad m, Forall (KeyValue KnownSymbol Glassy) xs)
  => Box V2 Float
  -> RecordOf Sized xs
  -> (forall x. Glassy (AssocValue x) => Membership xs x -> AssocValue x -> Box V2 Float -> m (h (AssocValue x)))
  -> m (RecordOf h xs)
withSubbox (Box (V2 x0 y0) (V2 x1 y1)) rec k = flip evalStateT y0
  $ hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol Glassy))
  $ \i -> StateT $ \y ->
    let (h, a) = case getField $ hindex rec i of
              Sized f x -> (f * height, x)
              Unsized x -> (freeRatio * height, x)
    in do
      s' <- k i a $ Box (V2 x0 y) (V2 x1 (y + h))
      return (Field s', y + h)
  where
    height = y1 - y0
    freeRatio = (1 - reserve) / count
    (Sum reserve, Sum count) = hfoldMap (\c -> case getField c of
      Sized f _ -> (Sum f, Sum 0)
      Unsized _ -> (mempty, Sum 1)) rec

newtype WrapEvent a = WrapEvent { unwrapEvent :: Event a }

instance Forall (KeyValue KnownSymbol Glassy) xs => Glassy (RowRec xs) where
  type State (RowRec xs) = RecordOf WrapState xs
  type Event (RowRec xs) = VariantOf WrapEvent xs
  initialState (RowRec rec) = htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol Glassy))
    $ \i -> Field $ WrapState $ initialState $ case getField $ hindex rec i of
      Sized _ a -> a
      Unsized a -> a
  poll (RowRec rec) = do
    box <- askEff #box
    states <- get
    (states', Endo act) <- runWriterT $ withSubbox box rec $ \i a box' -> do
      ((draw, s'), es) <- lift $ localEff #box (const box')
        $ castEff
        $ enumWriterEff
        $ poll a `runStateDef` unwrapState (getField $ hindex states i)
      tell $ Endo $ (>> runReaderEff draw box')
      mapM_ (lift . tellEff #event . EmbedAt i . Field . WrapEvent) es
      return $ WrapState s'
    put states'
    return $ castEff $ act $ return ()

instance (Glassy a, Glassy b) => Glassy (a, b) where
  type State (a, b) = (State a, State b)
  type Event (a, b) = Either (Event a) (Event b)
  initialState (a, b) = (initialState a, initialState b)

  poll (a, b) = do
    (s, t) <- get
    ((da, s'), es) <- castEff $ enumWriterEff @ "event" $ poll a `runStateDef` s
    ((db, t'), fs) <- castEff $ enumWriterEff @ "event" $ poll b `runStateDef` t
    put (s', t')
    mapM_ (tellEff #event) $ map Left es ++ map Right fs
    return (da >> db)

-- | Left mouse button
data LMB = LMB

instance Glassy LMB where
  type State LMB = Bool
  type Event LMB = ()
  initialState _ = False
  poll LMB = do
    f <- liftHolz $ mousePress 0
    b <- get
    put f
    box <- askEff #box
    pos <- liftHolz getCursorPos
    when (not b && f && Box.isInside pos box) $ tellEff #event ()
    return (return ())
