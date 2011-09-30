{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{- Main.hs; John Groszko (john@tinythunk.com) 2005

Main module

-}

module Main where

import Data.IORef
import System.Random
import Graphics.UI.GLUT

import FRP.Animas
import qualified FRP.Animas.Geometry as FRP

type Position2 = FRP.Point2 Float
type Velocity2 = FRP.Vector2 Float

clkRes :: Double
clkRes = 1000

xMin :: Float
xMin = -0.9

xMax :: Float
xMax = 0.9

stateToVector3 pos = let x = (realToFrac $ FRP.point2X pos)::GLfloat
                         y = (realToFrac $ FRP.point2Y pos)::GLfloat
                     in
                       Vector3 x y 0.0

data SphereState = SphereState { ssPos :: !Position2
                               , ssDPos :: !Position2
    }
                 deriving (Show)

idle :: (ReactHandle a b) -> IORef Int -> IO ()
idle rh tRef = do
  time <- get tRef
  currentTime <- get elapsedTime

  let dt = (fromIntegral (currentTime - time))/clkRes

  react rh (dt, Nothing)

  writeIORef tRef currentTime
  return ()

actuate :: ReactHandle a b -> Bool -> SphereState -> IO Bool
actuate _ _ noos  = do
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity

  preservingMatrix (do
                     translate $ stateToVector3 $ ssDPos noos
                     color (Color3 0.3 0.3 (0.3::GLfloat))
                     renderObject Solid (Sphere' 0.05 8 2)
                   )

  preservingMatrix (do
                     translate $ stateToVector3 $ ssPos noos
                     color (Color3 1.0 0.0 (0.0::GLfloat))
                     renderObject Solid (Sphere' 0.05 8 2)
                   )
  swapBuffers

  return False

initr :: IO ()
initr = do
  return ()

type SimpleSphere = SF () SphereState

simpleSphere :: (RandomGen g0, RandomGen g1) => g0 -> g1 -> Position2 -> SimpleSphere
simpleSphere gx gy (FRP.Point2 x0 y0) = proc gi -> do
                                      rec
                                               -- Pick a position
                                               smpl <- repeatedly 5 () -< ()
                                               (rx,ry) <- (noiseR (xMin, xMax) gx) &&& (noiseR (xMin, xMax) gy) -< ()
                                               dx <- hold (x0) -< smpl `tag` rx
                                               dy <- hold (y0) -< smpl `tag` ry

                                               let ax = 10 * (dx - x) - 8 * vx
                                               vx <- integral -< ax
                                               x <- (x0+) ^<< integral -< vx

                                               let ay = 10 * (dy - y) - 8 * vy
                                               vy <- integral -< ay
                                               y <- (y0+) ^<< integral -< vy

                                      returnA -< SphereState { ssPos = (FRP.Point2 x y)
                                                             , ssDPos = (FRP.Point2 dx dy)
                                                             }

main :: IO ()
main = do
  (progname, args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBAMode]
  initialWindowSize $= (Size 400 400)
  createWindow "Moving Spheres"

  t <- get elapsedTime
  timeRef <- newIORef t

  let iv = FRP.Point2 0 0
      gen_x = mkStdGen 10
      gen_y = mkStdGen 10

  rh <- reactInit (initr) (actuate) (simpleSphere gen_x gen_y iv)

  displayCallback $= (do return ())
  idleCallback $= Just (idle rh timeRef)
  mainLoop
