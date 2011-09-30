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
xMin = -5.0

xMax :: Float
xMax = 5.0

accFac :: Float
accFac = 10

velFac :: Float
velFac = 5

stateToVector3 pos z = let x = (realToFrac $ FRP.point2X pos)::GLfloat
                           y = (realToFrac $ FRP.point2Y pos)::GLfloat
                       in
                         Vector3 x y z

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

actuate :: ReactHandle a b -> Bool -> [SphereState] -> IO Bool
actuate _ _ noos  = do
  clear [ ColorBuffer, DepthBuffer ]

  mapM (\noo -> do
          preservingMatrix (do
                             translate $ stateToVector3 (ssDPos noo) 0.0
                             color (Color3 0.1 0.1 (0.1::GLfloat))
                             renderObject Solid (Sphere' 0.1 8 2)
                           )
          preservingMatrix (do
                             translate $ stateToVector3 (ssPos noo) 0.0
                             color (Color3 1.0 0.0 (0.0::GLfloat))
                             renderObject Solid (Sphere' 0.1 8 2)
                           )
       ) noos
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
                                               smpl <- repeatedly 3 () -< ()
                                               (rx,ry) <- (noiseR (xMin, xMax) gx) &&& (noiseR (xMin, xMax) gy) -< ()
                                               dx <- hold (x0) -< smpl `tag` rx
                                               dy <- hold (y0) -< smpl `tag` ry

                                               let ax = accFac * (dx - x) - velFac * vx
                                               vx <- integral -< ax
                                               x <- (x0+) ^<< integral -< vx

                                               let ay = accFac * (dy - y) - velFac * vy
                                               vy <- integral -< ay
                                               y <- (y0+) ^<< integral -< vy

                                      returnA -< SphereState { ssPos = (FRP.Point2 x y)
                                                             , ssDPos = (FRP.Point2 dx dy)
                                                             }

reshape :: ReshapeCallback
reshape size@(Size w h) = do
  let vp = 0.8
      aspect = fromIntegral w / fromIntegral h
  
  viewport $= (Position 0 0, size)

  matrixMode $= Projection
  loadIdentity
  frustum (-vp) vp (-vp / aspect) (vp / aspect) 3 50

  matrixMode $= Modelview 0
  loadIdentity
  translate (Vector3 0 0 (-20 :: GLfloat))

main :: IO ()
main = do
  (progname, args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBAMode]
  initialWindowSize $= (Size 400 400)
  createWindow "Moving Spheres"

  t <- get elapsedTime
  timeRef <- newIORef t

  rh <- reactInit (initr) (actuate) (parB (map (\i -> let g0 = mkStdGen (i*21903821093810 `mod` 200)
                                                          (x, g1) = randomR (xMin, xMax) g0
                                                          (y, g2) = randomR (xMin, xMax) g1
                                                          pos = FRP.Point2 x y
                                                      in
                                                        simpleSphere g1 g2 pos)
                                           [0..3]))

  displayCallback $= (do return ())
  idleCallback $= Just (idle rh timeRef)
  reshapeCallback $= Just reshape
  mainLoop
