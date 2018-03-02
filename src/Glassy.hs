{-# LANGUAGE BangPatterns, TypeFamilies, ScopedTypeVariables, Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, DefaultSignatures #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Glassy (Glassy(..)
  , start
  , GlassyConfig(..)
  , defaultGlassyConfig
  -- * Basic types
  , Str(..)
  , Glassy.Show(..)
  , Fill(..)
  , fillRGBA
  , Frame(..)
  , Self(..)
  , self
  -- * Transition
  , Transit(..)
  , TransitionState(..)
  , transits
  , transitIn
  , transitOut
  , transitState
  -- * Automata
  , Auto(..)
  , AutoState(..)
  , autoState
  -- * Collection
  , Rows(..)
  , Columns(..)
  , insertElem
  , ElemState(..)
  , elemState
  -- * Layout
  , Margin(..)
  , VRec(..)
  , HRec(..)
  , Sized(..)
  , WrapState(..)
  , WrapEvent(..)
  -- * Input
  , Always(..)
  , Chatter(..)
  , Key(..)
  , Hover(..)
  , LMB(..)
  , TextBox(..)
  , textBoxText
  , clearTextBox
  -- * reexport
  , module Glassy.Color)
  where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Writer
import qualified Data.BoundingBox as Box
import Data.Extensible hiding (State)
import Data.List (foldl')
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

data GlassyEnv = GlassyEnv
  { gShader :: !Shader
  , gWindow :: !Window
  , gFont :: !Text.Renderer
  , gBox :: !(Box V2 Float)
  }
instance HasWindow GlassyEnv where
  getWindow = gWindow
instance HasShader GlassyEnv where
  getShader = gShader

data GlassyConfig = GlassyConfig
  { framesPerSecond :: !Double
  , defaultSize :: V2 Float
  }

defaultGlassyConfig :: GlassyConfig
defaultGlassyConfig = GlassyConfig
  { framesPerSecond = 30
  , defaultSize = V2 640 480
  }

_event :: Proxy "event"
_event = Proxy

_close :: Proxy "close"
_close = Proxy

type HolzM = ReaderT GlassyEnv IO

class Glassy a where
  type State a
  type State a = ()
  type Event a
  type Event a = Void
  initialState :: a -> State a

  default initialState :: (State a ~ ()) => a -> State a
  initialState _ = ()

  poll :: a -> GlassyEffs (State a) (Event a) (HolzM ())
  poll _ = return $ return ()

start :: Glassy a => GlassyConfig -> a -> IO ()
start GlassyConfig{..} body = withHolz $ do
  win <- openWindow Resizable $ Box (V2 0 0) defaultSize
  sh <- makeShader
  font <- case Info.os of
    "linux" -> Text.typewriter "/usr/share/fonts/truetype/takao-gothic/TakaoPGothic.ttf"
    "darwin" -> Text.typewriter "/System/Library/Fonts/LucidaGrande.ttc"
    "windows" -> Text.typewriter "C:\\Windows\\Fonts\\segoeui.ttf"
    _ -> fail "Unsupported"
  let env = GlassyEnv
        { gShader = sh
        , gWindow = win
        , gFont = font
        , gBox = pure 0
        }
  flip fix (initialState body) $ \self' s -> join $ withFrame win $ flip runReaderT env $ do
    t0 <- liftIO getCurrentTime
    setOrthographic
    box <- getBoundingBox
    (cont, s', _) <- lift $ runRWST (poll body) env { gBox = box } s
    cont
    t1 <- liftIO getCurrentTime
    liftIO $ threadDelay $ floor $ (*1e6)
      $ 1 / framesPerSecond - (realToFrac (diffUTCTime t1 t0) :: Double)
    shouldClose <- runReaderT windowShouldClose win
    return $ if shouldClose then pure () else self' s'

type GlassyEffs s e = RWST GlassyEnv [e] s IO

data Str = Str RGBA String

drawStringIn :: (MonadHolz r m, HasShader r) => Box V2 Float -> Text.Renderer -> RGBA -> String -> m ()
drawStringIn (Box (V2 x0 y0) (V2 x1 y1)) font fg str = Text.runRenderer font $ do
  let size = (y1 - y0) * 2 / 3
  Text.string size fg str
  V2 x y <- Text.getOffset
  let k = min 1 $ (x1 - x0) / x
  Text.render $ translate (V3 (x1 - 4 - k * x) (y0 + (y1 - y0) * 0.75 - k * y) 1)
    !*! scaled (V4 k k k 1)
  Text.clear

instance Glassy Str where
  type State Str = String
  initialState (Str _ s) = s
  poll (Str fg _) = do
    box <- asks gBox
    s <- get
    return $ do
      font <- asks gFont
      drawStringIn box font fg s

-- | Hide overflow
newtype Frame a = Frame { getFrame :: a }

instance Glassy a => Glassy (Frame a) where
  type State (Frame a) = State a
  type Event (Frame a) = Event a
  initialState (Frame a) = initialState a
  poll (Frame a) = do
    box@(Box (V2 x0 y0) (V2 x1 y1)) <- asks gBox
    m <- poll a
    return $ do
      do
        setViewport box
        setProjection $ ortho x0 x1 y1 y0 (-1) 1
      m
      setOrthographic

newtype Show a = Show { getShow :: a }
  deriving (Bounded, Enum, Eq, Floating, Fractional, Integral, Monoid, Num, Ord
    , Real, RealFrac, RealFloat)

instance Prelude.Show a => Glassy (Glassy.Show a) where
  type State (Glassy.Show a) = a
  initialState (Show a) = a
  poll _ = do
    box <- asks gBox
    a <- get
    return $ do
      font <- asks gFont
      drawStringIn box font (pure 1) $ show a

newtype Fill = Fill { fillColor :: V4 Float } deriving Transitive

instance Glassy Fill where
  poll (Fill bg) = do
    Box p q <- asks gBox
    return $ draw identity $ rectangle (bg & _xyz %~ fromHSV) p q

fillRGBA :: Float -> Float -> Float -> Float -> Fill
fillRGBA r g b a = Fill $ let V3 h s v = toHSV (V3 r g b) in V4 h s v a

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
  => Bool -> RecordOf Sized xs -> GlassyEffs (RecordOf WrapState xs)
    (VariantOf WrapEvent xs) (HolzM ())
pollRec horiz rec = do
  box <- asks gBox
  states <- get
  (states', Endo act) <- runWriterT $ withSubbox horiz box rec $ \i a box' -> do
    env <- ask
    (m, s', es) <- lift $ lift
        $ runRWST (poll a) env {gBox = box'}
        $ unwrapState (getField $ hindex states i)
    tell $ Endo $ (>>m)
    mapM_ (lift . tell . pure . EmbedAt i . Field . WrapEvent) es
    return $ WrapState s'
  put states'
  return $ act $ return ()

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

data Auto w a = Auto
  { autoWatch :: w -- ^ the target to watch
  , autoView :: a -- ^ display
  , autoUpdate :: Event w -> State a -> State a -- update its own state.
  }

data Margin a = MarginTRBL !Float !Float !Float !Float a

instance Glassy a => Glassy (Margin a) where
  type State (Margin a) = State a
  type Event (Margin a) = Event a
  initialState (MarginTRBL _ _ _ _ a) = initialState a
  poll (MarginTRBL t r b l a) = local
    (\e -> let V2 x0 y0 `Box` V2 x1 y1 = gBox e
      in e { gBox = V2 (x0 + l) (y0 + t) `Box` V2 (x1 - r) (y1 - b) })
    (poll a)

data AutoState w a = AutoState !(State w) (State a)

instance (Glassy w, Glassy a) => Glassy (Auto w a) where
  type State (Auto w a) = AutoState w a
  type Event (Auto w a) = Event a
  initialState (Auto w a _) = AutoState (initialState w) (initialState a)
  poll (Auto w v u) = do
    env <- ask
    AutoState ws vs <- get
    (n, ws', es) <- lift $ runRWST (poll w) env ws
    let !vs' = foldr u vs es
    (m, vs'', os) <- lift $ runRWST (poll v) env vs'
    put $ AutoState ws' vs''
    tell os
    return (n >> m)

autoState :: Lens' (AutoState w a) (State a)
autoState f (AutoState w a) = AutoState w <$> f a

insertElem :: Glassy a => a -> [ElemState a] -> [ElemState a]
insertElem a = flip snoc $ ElemState a (initialState a)

data ElemState a = ElemState !a !(State a)

elemState :: Lens' (ElemState a) (State a)
elemState f (ElemState a s) = ElemState a <$> f s

data Rows a = Rows

data Columns a = Columns

instance Glassy a => Glassy (Rows a) where
  type State (Rows a) = [ElemState a]
  type Event (Rows a) = Event a
  initialState _ = []
  poll Rows = do
    env <- ask
    Box (V2 x0 y0) (V2 x1 y1) <- asks gBox
    ss <- get
    let h = (y1 - y0) / fromIntegral (length ss)
    let ys = [y0, y0+h..]
    (ms, ss', os) <- lift $ fmap unzip3 $ forM (zip3 ys (tail ys) ss)
      $ \(y, y', ElemState a s) -> runRWST (poll a)
        env { gBox = Box (V2 x0 y) (V2 x1 y') } s
    mapM_ tell os
    put $ zipWith (\(ElemState a _) s -> ElemState a s) ss ss'
    return $ sequence_ ms

instance Glassy a => Glassy (Columns a) where
  type State (Columns a) = [ElemState a]
  type Event (Columns a) = Event a
  initialState _ = []
  poll Columns = do
    env <- ask
    Box (V2 x0 y0) (V2 x1 y1) <- asks gBox
    ss <- get
    let h = (x1 - x0) / fromIntegral (length ss)
    let xs = [x0, x0+h..]
    (ms, ss', os) <- lift $ fmap unzip3 $ forM (zip3 xs (tail xs) ss)
      $ \(x, x', ElemState a s) -> runRWST (poll a)
        env { gBox = Box (V2 x y0) (V2 x' y1) } s
    mapM_ tell os
    put $ zipWith (\(ElemState a _) s -> ElemState a s) ss ss'
    return $ sequence_ ms

newtype Self a = Self a

instance Glassy a => Glassy (Self a) where
  type State (Self a) = (a, State a)
  type Event (Self a) = Event a
  initialState (Self a) = (a, initialState a)
  poll (Self _) = do
    (a, s) <- get
    env <- ask
    (m, s', e) <- liftIO $ runRWST (poll a) env s
    put (a, s')
    tell e
    return m

-- | Accessor for the 'Self' state
self :: Lens' (a, b) a
self = _1

instance (Glassy a, Glassy b) => Glassy (a, b) where
  type State (a, b) = (State a, State b)
  type Event (a, b) = Either (Event a) (Event b)
  initialState (a, b) = (initialState a, initialState b)

  poll (a, b) = do
    env <- ask
    (s, t) <- get
    (da, s', es) <- lift $ runRWST (poll a) env s
    (db, t', fs) <- lift $ runRWST (poll b) env t
    put (s', t')
    tell $ map Left es ++ map Right fs
    return (da >> db)

instance Glassy Key where
  type State Key = Bool
  type Event Key = Bool
  initialState _ = False
  poll k = do
    f <- keyPress k
    b <- get
    put f
    when (b /= f) $ tell [f]
    return (return ())

-- | Left mouse button
data LMB = LMB

instance Glassy LMB where
  type State LMB = Bool
  type Event LMB = Bool
  initialState _ = False
  poll LMB = do
    f <- mousePress 0
    b <- get
    put f
    box <- asks gBox
    pos <- getCursorPos
    when (b /= f && Box.isInside pos box) $ tell [f]
    return (return ())

data Hover = Hover

instance Glassy Hover where
  type State Hover = Bool
  type Event Hover = Bool
  initialState _ = False
  poll Hover = do
    b <- get
    box <- asks gBox
    f <- (`Box.isInside` box) <$> getCursorPos
    put f
    when (b /= f) $ tell [f]
    return $ return ()

instance (Event a ~ Bool, Glassy a) => Glassy (Chatter a) where
  type State (Chatter a) = State a
  type Event (Chatter a) = ()
  initialState (Up a) = initialState a
  initialState (Down a) = initialState a
  poll t = do
    env <- ask
    let (cond, a) = case t of
          Up x -> (any not, x)
          Down x -> (or, x)
    s <- get
    (m, s', es) <- lift $ runRWST (poll a) env s
    when (cond es) $ tell [()]
    put s'
    return m

data Always = Always

instance Glassy Always where
  type Event Always = ()
  poll _ = return () <$ tell [()]

data TextBox = TextBox

instance Glassy TextBox where
  type State TextBox = Either String (String, Int)
  initialState TextBox = Left ""
  poll TextBox = do
    box <- asks gBox
    cursorIsIn <- (`Box.isInside` box) <$> getCursorPos
    btn <- mousePress 0
    env <- ask
    get >>= \case
      Left str -> do
        when (btn && cursorIsIn) $ put $ Right (str, length str)
        (m, s', os) <- lift $ runRWST (poll (Str (pure 1) str)) env str
        put (Left s')
        tell os
        return m
      Right s@(str, _)
        | btn && not cursorIsIn -> do
          put (Left str)
          (m, s', os) <- lift $ runRWST (poll (Str (pure 1) str)) env str
          put (Left s')
          tell os
          return m
        | otherwise -> do
          (m, s', os) <- lift $ runRWST activeTextBox env s
          put (Right s')
          tell os
          return m

textBoxText :: State TextBox -> String
textBoxText (Left s) = s
textBoxText (Right (s, _)) = s

clearTextBox :: State TextBox -> State TextBox
clearTextBox (Left _) = Left ""
clearTextBox (Right _) = Right ("", 0)

activeTextBox :: GlassyEffs (String, Int) Void (HolzM ())
activeTextBox = do
  Box (V2 x0 y0) (V2 x1 y1) <- asks gBox
  xs <- typedString
  ks <- typedKeys
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
  put (str', max 0 $ min (length str') $ p')
  font <- asks gFont
  return $ font `Text.runRenderer` do
    let fg = pure 1
    let size = (y1 - y0) * 2 / 3
    let (sl, sr) = splitAt p' str'
    Text.string size fg sl
    cursor <- Text.getOffset
    Text.string size fg sr
    V2 x y <- Text.getOffset
    let c = min 1 $ (x1 - x0) / x
    let mat = translate (V3 (x0 + 4) (y0 + (y1 - y0) * 0.75 - c * y) 1)
          !*! scaled (V4 c c c 1)

    -- Draw a bar
    lift $ draw (mat !*! (identity & translation . _xy .~ cursor))
      $ rectangle (V4 0.5 0.5 0.5 0.8) (V2 (-size/24) (-size)) (V2 (size/24) 0)

    Text.render mat
    Text.clear

-- | transit with a shared state
data Transit a = Transit !Int (Float -> a)

data TransitionState = TBeginning | TIn !Float | TOut !Float | TEnd deriving (Eq, Ord)

transitIn :: (TransitionState, a) -> (TransitionState, a)
transitIn (TOut i, a) = (TIn i, a)
transitIn (_, a) = (TIn 0, a)

transitOut :: (TransitionState, a) -> (TransitionState, a)
transitOut (TIn i, a) = (TOut i, a)
transitOut (_, a) = (TOut 1, a)

transitState :: Lens' (TransitionState, a) TransitionState
transitState = _1

instance (Glassy a) => Glassy (Transit a) where
  type State (Transit a) = (TransitionState, State a)
  type Event (Transit a) = Event a
  initialState (Transit _ f) = (TBeginning, initialState $ f 0)
  poll (Transit dur f) = do
    env <- ask
    get >>= \case
      (TBeginning, s) -> do
        (m, s', os) <- lift $ runRWST (poll $ f 0) env s
        put (TBeginning, s')
        tell os
        return m
      (TIn k, s) -> do
        (m, s', os) <- lift $ runRWST (poll $ f k) env s
        if k < 1
          then put (TIn (k + 1 / fromIntegral dur), s')
          else put (TEnd, s')
        tell os
        return m
      (TOut k, s) -> do
        (m, s', os) <- lift $ runRWST (poll $ f k) env s
        if k > 0
          then put (TOut (k - 1 / fromIntegral dur), s')
          else put (TBeginning, s')
        tell os
        return m
      (TEnd, s) -> do
        (m, s', os) <- lift $ runRWST (poll $ f 1) env s
        put (TEnd, s')
        tell os
        return m
