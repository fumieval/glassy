{-# LANGUAGE BangPatterns, TypeFamilies, ScopedTypeVariables, Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, DefaultSignatures #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
module Glassy (Glassy(..)
  , start
  , liftHolz
  -- * Basic types
  , Str(..)
  , Glassy.Show(..)
  , Fill(..)
  , rgb
  , Frame(..)
  , Transit(..)
  , TransitionState(..)
  , transitIn
  , transitOut
  -- * Automata
  , Auto(..)
  -- * Layout
  , Rows(..)
  , Margin(..)
  , VRec(..)
  , HRec(..)
  , Sized(..)
  , WrapState(..)
  , WrapEvent(..)
  -- * Input
  , Key(..)
  , Hover(..)
  , LMB(..)
  , TextBox(..))
  where

import Control.Concurrent (threadDelay)
import Control.Lens
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
import Data.List (foldl', zip4)
import Data.Proxy
import Data.Time.Clock
import Data.Void
import GHC.TypeLits
import Graphics.Holz
import Linear
import qualified Graphics.Holz.Text as Text
import qualified System.Info as Info
import Glassy.Color
import Glassy.Transitive

type HolzEffs =
  [ "holzShader" >: ReaderEff Shader
  , "holzWindow" >: ReaderEff Window
  , "font" >: ReaderEff Text.Renderer
  , "IO" >: IO]

type GlassyEffs s e = StateDef s
    ': ("event" >: WriterEff e)
    ': ("box" >: ReaderEff (Box V2 Float))
    ': HolzEffs

class Glassy a where
  type State a
  type State a = ()
  type Event a
  type Event a = Void
  initialState :: a -> State a

  default initialState :: a -> ()
  initialState _ = ()

  poll :: a -> Eff (GlassyEffs (State a) (Event a)) (Eff HolzEffs ())
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
          m <- pipeWriterEff @ "event" (const $ return ()) $ castEff $ poll a
          castEff m
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
  poll (Str str) = do
    Box (V2 x0 y0) (V2 x1 y1) <- askEff #box
    return $ do
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

-- | Hide overflow
newtype Frame a = Frame { getFrame :: a }

instance Glassy a => Glassy (Frame a) where
  type State (Frame a) = State a
  type Event (Frame a) = Event a
  initialState (Frame a) = initialState a
  poll (Frame a) = do
    box@(Box (V2 x0 y0) (V2 x1 y1)) <- askEff #box
    m <- poll a
    return $ do
      liftHolz $ do
        setViewport box
        setProjection $ ortho x0 x1 y1 y0 (-1) 1
      m
      liftHolz setOrthographic

newtype Rows a = Rows { getRows :: [a] }

instance Glassy a => Glassy (Rows a) where
  type State (Rows a) = [State a]
  type Event (Rows a) = Event a
  initialState = map initialState . getRows
  poll (Rows xs) = do
    Box (V2 x0 y0) (V2 x1 y1) <- askEff #box
    let h = (y1 - y0) / fromIntegral (length xs)
    _ss <- get
    let ys = [y0, y0+h..]
    (ms, ss') <- fmap unzip $ forM (zip4 ys (tail ys) xs $ map initialState xs) -- FIXME
      $ \(y, y', a, s) -> castEff
        $ localEff #box (const $ Box (V2 x0 y) (V2 x1 y'))
        $ poll a `runStateDef` s
    put ss'
    return $ sequence_ ms

newtype Show a = Show { getShow :: a }
  deriving (Bounded, Enum, Eq, Floating, Fractional, Integral, Monoid, Num, Ord
    , Real, RealFrac, RealFloat)

instance Prelude.Show a => Glassy (Glassy.Show a) where
  poll (Show a) = poll $ Str $ show a

newtype Fill = Fill { fillColor :: V4 Float } deriving Transitive

instance Glassy Fill where
  poll (Fill bg) = do
    Box p q <- askEff #box
    return $ liftHolz $ draw identity $ rectangle (bg & _xyz %~ fromHSV) p q

rgb :: Float -> Float -> Float -> V4 Float
rgb r g b = V4 r g b 1

instance Glassy a => Glassy (Eff HolzEffs a) where
  type State (Eff HolzEffs a) = Maybe (State a)
  type Event (Eff HolzEffs a) = Event a
  initialState _ = Nothing
  poll m = do
    a <- castEff m
    s <- maybe (initialState a) id <$> get
    (n, s') <- castEff $ runStateDef (poll a) s
    put $ Just s'
    return n

data Auto s w a = Auto
  { autoInitial :: s -- ^ the initial state
  , autoWatch :: w -- ^ the target to watch
  , autoView :: s -> a -- ^ display
  , autoUpdate :: Event w -> (s, State a) -> (s, State a) -- update its own state.
  }

pipeWriterEff :: forall k w xs a. (w -> Eff xs ())
  -> Eff (k >: WriterEff w ': xs) a
  -> Eff xs a
pipeWriterEff p = peelEff0 return $ \(w, a) k -> p w >> k a

enumWriterEff :: forall k w xs a. Eff (k >: WriterEff w ': xs) a
  -> Eff xs (a, [w])
enumWriterEff = peelEff1 (\a k -> return (a, k [])) (\(w, a) k f -> k a $ (w:) . f)
  `flip` id

instance (Glassy w, Glassy a) => Glassy (Auto s w a) where
  type State (Auto s w a) = (s, State w, State a)
  type Event (Auto s w a) = Event a
  initialState (Auto s w f _) = (s, initialState w, initialState $ f s)
  poll (Auto _ w f u) = do
    (s, ws, ts) <- get
    ((n, ws'), es) <- castEff $ enumWriterEff $ poll w `runStateDef` ws
    ((m, ts'), os) <- castEff $ enumWriterEff $ poll (f s) `runStateDef` ts
    let (!s', !ts'') = foldr u (s, ts') es
    put (s', ws', ts'')
    mapM_ (tellEff #event) os
    return (n >> m)

newtype VRec (xs :: [Assoc Symbol *]) = VRec { getVRec :: RecordOf Sized xs }

data Sized a = Sized !Float !a | Unsized !a

instance Wrapper Sized where
  type Repr Sized a = a
  _Wrapper = dimap (\case
    Sized _ a -> a
    Unsized a -> a) (fmap Unsized)

newtype WrapState a = WrapState { unwrapState :: State a }

instance Wrapper WrapState where
  type Repr WrapState a = State a
  _Wrapper = dimap unwrapState (fmap WrapState)

withSubbox :: (Monad m, Forall (KeyValue KnownSymbol Glassy) xs)
  => Bool -- horizontal?
  -> Box V2 Float
  -> RecordOf Sized xs
  -> (forall x. Glassy (AssocValue x) => Membership xs x -> AssocValue x -> Box V2 Float -> m (h (AssocValue x)))
  -> m (RecordOf h xs)
withSubbox horiz (Box (V2 x0 y0) (V2 x1 y1)) rec k = flip evalStateT
  (if horiz then x0 else y0)
  $ hgenerateFor (Proxy :: Proxy (KeyValue KnownSymbol Glassy))
  $ \i -> StateT $ \t ->
    let (d, a) = case getField $ hindex rec i of
              Sized f x -> (f * total, x)
              Unsized x -> (freeRatio * total, x)
    in do
      s' <- k i a $ if horiz
        then Box (V2 t y0) (V2 (t + d) y1)
        else Box (V2 x0 t) (V2 x1 (t + d))
      return (Field s', t + d)
  where
    total = if horiz then x1 - x0 else y1 - y0
    freeRatio = (1 - reserve) / count
    (Sum reserve, Sum count) = hfoldMap (\c -> case getField c of
      Sized f _ -> (Sum f, Sum 0)
      Unsized _ -> (mempty, Sum 1)) rec

newtype WrapEvent a = WrapEvent { unwrapEvent :: Event a }

instance Wrapper WrapEvent where
  type Repr WrapEvent a = Event a
  _Wrapper = dimap unwrapEvent (fmap WrapEvent)

initRec :: Forall (KeyValue KnownSymbol Glassy) xs
  => RecordOf Sized xs -> RecordOf WrapState xs
initRec rec = htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol Glassy))
  $ \i -> Field $ WrapState $ initialState $ case getField $ hindex rec i of
    Sized _ a -> a
    Unsized a -> a

pollRec :: Forall (KeyValue KnownSymbol Glassy) xs
  => Bool -> RecordOf Sized xs -> Eff (GlassyEffs (RecordOf WrapState xs)
    (VariantOf WrapEvent xs)) (Eff HolzEffs ())
pollRec horiz rec = do
  box <- askEff #box
  states <- get
  (states', Endo act) <- runWriterT $ withSubbox horiz box rec $ \i a box' -> do
    ((m, s'), es) <- lift $ localEff #box (const box')
      $ castEff
      $ enumWriterEff
      $ poll a `runStateDef` unwrapState (getField $ hindex states i)
    tell $ Endo $ (>>m)
    mapM_ (lift . tellEff #event . EmbedAt i . Field . WrapEvent) es
    return $ WrapState s'
  put states'
  return $ castEff $ act $ return ()

instance Forall (KeyValue KnownSymbol Glassy) xs => Glassy (VRec xs) where
  type State (VRec xs) = RecordOf WrapState xs
  type Event (VRec xs) = VariantOf WrapEvent xs
  initialState = initRec . getVRec
  poll = pollRec False . getVRec

newtype HRec (xs :: [Assoc Symbol *]) = HRec { getHRec :: RecordOf Sized xs }

instance Forall (KeyValue KnownSymbol Glassy) xs => Glassy (HRec xs) where
  type State (HRec xs) = RecordOf WrapState xs
  type Event (HRec xs) = VariantOf WrapEvent xs
  initialState = initRec . getHRec
  poll = pollRec True . getHRec

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

data Margin a = MarginTRBL !Float !Float !Float !Float a

instance Glassy a => Glassy (Margin a) where
  type State (Margin a) = State a
  type Event (Margin a) = Event a
  initialState (MarginTRBL _ _ _ _ a) = initialState a
  poll (MarginTRBL t r b l a) = localEff #box
    (\(V2 x0 y0 `Box` V2 x1 y1) -> V2 (x0 + l) (y0 + t) `Box` V2 (x1 - r) (y1 - b))
    (poll a)

instance Glassy Key where
  type State Key = Bool
  type Event Key = Bool
  initialState _ = False
  poll k = do
    f <- liftHolz $ keyPress k
    b <- get
    put f
    when (b /= f) $ tellEff #event f
    return (return ())

-- | Left mouse button
data LMB = LMB

instance Glassy LMB where
  type State LMB = Bool
  type Event LMB = Bool
  initialState _ = False
  poll LMB = do
    f <- liftHolz $ mousePress 0
    b <- get
    put f
    box <- askEff #box
    pos <- liftHolz getCursorPos
    when (b /= f && Box.isInside pos box) $ tellEff #event f
    return (return ())

data Hover = Hover

instance Glassy Hover where
  type State Hover = Bool
  type Event Hover = Bool
  initialState _ = False
  poll Hover = do
    b <- get
    box <- askEff #box
    f <- (`Box.isInside` box) <$> liftHolz getCursorPos
    put f
    when (b /= f) $ tellEff #event f
    return $ return ()

data TextBox = TextBox

instance Glassy TextBox where
  type State TextBox = Either String (String, Int)
  initialState TextBox = Left ""
  poll TextBox = do
    box <- askEff #box
    cursorIsIn <- (`Box.isInside` box) <$> liftHolz getCursorPos
    btn <- liftHolz $ mousePress 0
    get >>= \case
      Left str -> do
        when (btn && cursorIsIn) $ put $ Right (str, length str)
        castEff $ poll (Str str) `evalStateDef` ()
      Right s@(str, _)
        | btn && not cursorIsIn -> do
          put (Left str)
          castEff $ poll (Str str) `evalStateDef` ()
        | otherwise -> do
          (m, s') <- castEff $ activeTextBox `runStateDef` s
          put (Right s')
          return m

activeTextBox :: Eff (GlassyEffs (String, Int) Void) (Eff HolzEffs ())
activeTextBox = do
  Box (V2 x0 y0) (V2 x1 y1) <- askEff #box
  xs <- liftHolz typedString
  ks <- liftHolz typedKeys
  (str, p) <- get
  let move (V3 i j k) KeyBackspace = V3 (i + 1) j k
      move (V3 i j k) KeyDelete = V3 i (j + 1) k
      move (V3 i j k) KeyLeft = V3 i j (k - 1)
      move (V3 i j k) KeyRight = V3 i j (k + 1)
      move (V3 i j _) KeyHome = V3 i j 0
      move (V3 i j _) KeyEnd = V3 i j (length str)
      move v _ = v
  let V3 i j k = foldl' move (V3 0 0 p) ks
  let (l, r) = splitAt (p - i) str
  let str' = l ++ xs ++ drop (i + j) r
  let p' = length xs + k - i - j
  put (str', p')
  font <- askEff #font
  return $ liftHolz $ font `Text.runRenderer` do
    let fg = pure 1
    let size = (y1 - y0) * 2 / 3
    let (sl, sr) = splitAt p' str'
    Text.string size fg sl
    cursor <- Text.getOffset
    Text.string size fg sr
    V2 x y <- Text.getOffset
    let c = min 1 $ (x1 - x0) / x
    let mat = translate (V3 (x1 - 4 - c * x) (y0 + (y1 - y0) * 0.75 - c * y) 1)
          !*! scaled (V4 c c c 1)

    -- Draw a bar
    lift $ draw (mat !*! (identity & translation . _xy .~ cursor))
      $ rectangle (V4 0.5 0.5 0.5 0.8) (V2 (-size/24) (-size)) (V2 (size/24) 0)

    Text.render mat
    Text.clear

-- | transit with a shared state
data Transit a = Transit !Int !a !a

data TransitionState = TLeft | TIn !Float | TOut !Float | TRight

transitIn :: (TransitionState, a) -> (TransitionState, a)
transitIn (TOut i, a) = (TIn i, a)
transitIn (_, a) = (TIn 0, a)

transitOut :: (TransitionState, a) -> (TransitionState, a)
transitOut (TIn i, a) = (TOut i, a)
transitOut (_, a) = (TOut 1, a)

instance (Glassy a, Transitive a) => Glassy (Transit a) where
  type State (Transit a) = (TransitionState, State a)
  type Event (Transit a) = Event a
  initialState (Transit _ a _) = (TLeft, initialState a)
  poll (Transit dur a b) = get >>= \case
    (TLeft, s) -> do
      (m, s') <- castEff $ runStateEff (poll a) s
      put (TLeft, s')
      return m
    (TIn k, s) -> do
      (m, s') <- castEff $ runStateEff (poll $ transit k a b) s
      if k < 1
        then put (TIn (k + 1 / fromIntegral dur), s')
        else put (TRight, s')
      return m
    (TOut k, s) -> do
      (m, s') <- castEff $ runStateEff (poll $ transit k a b) s
      if k > 0
        then put (TOut (k - 1 / fromIntegral dur), s')
        else put (TLeft, s')
      return m
    (TRight, s) -> do
      (m, s') <- castEff $ runStateEff (poll b) s
      put (TRight, s')
      return m
