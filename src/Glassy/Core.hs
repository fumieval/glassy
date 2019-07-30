{-# LANGUAGE RecordWildCards #-}
module Glassy.Core where
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Time.Clock
import Graphics.Holz
import Linear
import qualified Graphics.Holz.Text as Text
import qualified System.Info as Info
import Glassy.Color

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

withBox :: Box V2 Float -> GlassyM a -> GlassyM a
withBox b = local $ \g -> g { gBox = b }

data GlassyConfig = GlassyConfig
  { framesPerSecond :: !Double
  , defaultSize :: V2 Float
  }

defaultGlassyConfig :: GlassyConfig
defaultGlassyConfig = GlassyConfig
  { framesPerSecond = 30
  , defaultSize = V2 640 480
  }

type GlassyM = ReaderT GlassyEnv IO

start :: GlassyConfig -> App -> IO ()
start GlassyConfig{..} app = withHolz $ do
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
  flip fix app $ \self' (App poll draw') -> join $ withFrame win $ flip runReaderT env $ do
    t0 <- liftIO getCurrentTime
    setOrthographic
    box <- getBoundingBox
    withBox box draw'
    t1 <- liftIO getCurrentTime
    liftIO $ threadDelay $ floor $ (*1e6)
      $ 1 / framesPerSecond - (realToFrac (diffUTCTime t1 t0) :: Double)
    app' <- withBox box poll
    shouldClose <- runReaderT windowShouldClose win
    return $ if shouldClose then pure () else self' app'

data App = App
  { pollApp :: GlassyM App
  , drawApp :: GlassyM ()
  }

drawStringIn :: (MonadHolz r m, HasShader r) => Box V2 Float -> Text.Renderer -> RGBA -> String -> m ()
drawStringIn (Box (V2 x0 y0) (V2 x1 y1)) font fg str = Text.runRenderer font $ do
  let size = (y1 - y0) * 2 / 3
  Text.string size fg str
  V2 x y <- Text.getOffset
  let k = min 1 $ (x1 - x0) / x
  Text.render $ translate (V3 (x1 - 4 - k * x) (y0 + (y1 - y0) * 0.75 - k * y) 1)
    !*! scaled (V4 k k k 1)
  Text.clear
