{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards    #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module World where

import Config
import Julia
import Palette

import Graphics.Gloss.Accelerate.Data.Picture                       as G
import Graphics.Gloss.Interface.Pure.Game                           hiding ( Vector, translate, scale )

import Data.Array.Accelerate                                        ( Array, Scalar, Vector, Exp, DIM2, Elt, Z(..), (:.)(..) )
import Data.Array.Accelerate.Data.Complex
import Data.Array.Accelerate.Examples.Internal                      as A
import qualified Data.Array.Accelerate                              as A

import Data.Char
import Data.Int
import Data.Label
import Data.Word
import System.Exit
import Text.Printf
import Prelude                                                      as P


-- World state
-- -----------

type R = Double

data World = World
  { worldPicture      :: !Picture
  , worldAnimating    :: !Bool
  , worldDirty        :: !Bool
  , worldRender       :: !(Scalar Float -> Scalar R -> Scalar R -> Scalar R -> Scalar Int32 -> Scalar R -> Array DIM2 Word32)
  , worldPalette      :: !(Vector Word32)
  , worldSizeX        :: !Int
  , worldSizeY        :: !Int
  , worldTime         :: !(Scalar Float)
  , worldSpeed        :: !(Scalar Float)
  , worldPosX         :: !(Scalar R)
  , worldPosY         :: !(Scalar R)
  , worldWidth        :: !(Scalar R)
  , worldRadius       :: !(Scalar R)
  , worldIters        :: !(Scalar Int32)
  , worldPanning      :: !(Maybe (Float, Float))
  , worldZooming      :: !(Maybe Double)
  }

initialWorld :: Config -> Options -> World
initialWorld conf opts
  = loadPreset opts 0
  $ World
      { worldDirty        = True
      , worldAnimating    = True
      , worldPicture      = blank
      , worldRender       = \_ _ _ _ _ _ -> A.fromList (Z:.0:.0) []
      , worldPalette      = run (get optBackend opts) (ultraPalette 2048)
      , worldSizeX        = get configWidth conf
      , worldSizeY        = get configHeight conf
      , worldPanning      = Nothing
      , worldZooming      = Nothing
      , worldTime         = unit' 0
      , worldSpeed        = unit' 1
      , worldPosX         = unit' 0
      , worldPosY         = unit' 0
      , worldWidth        = unit' 0
      , worldRadius       = unit' 0
      , worldIters        = unit' 0
      }


-- Draw the world
--
draw :: World -> IO Picture
draw = return . worldPicture

-- React to events
--
react :: Config -> Options -> Event -> World -> IO World
react conf opts event world@World{..} =
  case event of
    -- zooming
    EventKey (Char 'w') s _ _               -> toggle zooming 0.975 s world
    EventKey (Char 's') s _ _               -> toggle zooming 1.025 s world

    -- panning
    EventKey (MouseButton LeftButton) s _ p -> toggle panning p s world
    EventMotion (x,y)
      | Just (x0,y0) <- worldPanning
      -> let dx = (x0-x) * P.realToFrac (the' worldWidth) / P.fromIntegral worldSizeX
             dy = (y0-y) * P.realToFrac (the' worldWidth) / P.fromIntegral worldSizeX
         in
         return . dirty
                $ world { worldPosX    = unit' (the' worldPosX + P.realToFrac dx)
                        , worldPosY    = unit' (the' worldPosY - P.realToFrac dy)
                        , worldPanning = Just (x,y)
                        }

    -- algorithm
    EventKey (Char 'a') Down _ _ -> return . dirty
      $ world { worldIters = unit' (P.truncate (P.fromIntegral (the' worldIters) * 0.8 :: Double)) }
    EventKey (Char 'd') Down _ _ -> return . dirty
      $ world { worldIters = unit' (P.truncate (P.fromIntegral (the' worldIters) * 1.2 :: Double)) }

    EventKey (Char 'z') Down _ _ -> return . dirty
      $ world { worldRadius = unit' (the' worldRadius * 0.5) }
    EventKey (Char 'c') Down _ _ -> return . dirty
      $ world { worldRadius = unit' (the' worldRadius * 2.0) }

    EventKey (Char 'q') Down _ _ -> return . dirty
      $ world { worldSpeed = unit' (the' worldSpeed * 0.75) }
    EventKey (Char 'e') Down _ _ -> return . dirty
      $ world { worldSpeed = unit' (the' worldSpeed * 1.25) }

    -- animation
    EventKey (SpecialKey KeyLeft) Down _ _  -> return . dirty
      $ world { worldTime = unit' (the' worldTime - 1/60 * the' worldSpeed) }
    EventKey (SpecialKey KeyRight) Down _ _ -> return . dirty
      $ world { worldTime = unit' (the' worldTime + 1/60 * the' worldSpeed) }

    -- presets
    EventKey (Char d) Down _ _ | isDigit d  -> return . dirty $ loadPreset opts (read [d]) world

    -- misc
    EventKey (Char '.') Down _ _            -> putStrLn (showWorld world) >> return world
    EventKey (Char 'r') Down _ _            -> return $ (initialWorld conf opts) { worldAnimating = worldAnimating }
    EventKey (SpecialKey KeySpace) Down _ _ -> return $ world { worldAnimating = not worldAnimating }
    EventKey (SpecialKey KeyEsc) Down _ _   -> exitSuccess

    _                                       -> return world
  where
    toggle f x Down = return . dirty . set f (Just x)
    toggle f _ Up   = return         . set f Nothing

    dirty w = w { worldDirty = True }


-- Move and zoom the display based on key state
--
advance :: Float -> World -> IO World
advance dt = return . updateWorld . animate . zoom
  where
    animate world@World{..}
      | worldAnimating          = world { worldDirty = True, worldTime = unit' (dt * the' worldSpeed + the' worldTime) }
      | otherwise               = world
    --
    zoom world@World{..}
      | Just f <- worldZooming  = world { worldDirty = True, worldWidth = unit' (P.realToFrac f * the' worldWidth)  }
      | otherwise               = world

-- Update the picture
--
updateWorld :: World -> World
updateWorld world@World{..} =
  if worldDirty
    then world { worldDirty   = False
               , worldPicture = bitmapOfArray (renderWorld world) (not worldAnimating)
               }
    else world

renderWorld :: World -> Array DIM2 Word32
renderWorld World{..} =
  let !r = worldRender worldTime worldPosX worldPosY worldWidth worldIters worldRadius
   in r


-- Miscellaneous
-- -------------

zooming :: World :-> Maybe Double
zooming = lens worldZooming (\f World{..} -> World { worldZooming = f worldZooming, .. })

panning :: World :-> Maybe (Float,Float)
panning = lens worldPanning (\f World{..} -> World { worldPanning = f worldPanning, .. })

unit' :: Elt a => a -> Scalar a
unit' a = A.fromList Z [a]

the' :: Elt a => Scalar a -> a
the' a = a `A.indexArray` Z


-- Presets
-- -------

showWorld :: World -> String
showWorld World{..} =
  printf "centre: (%f, %f), width: %f, iterations: %d, radius: %f"
    (the' worldPosX)
    (the' worldPosY)
    (the' worldWidth)
    (the' worldIters)
    (the' worldRadius)

loadPreset :: Options -> Int -> World -> World
loadPreset opts n world@World{..} =
  let
      (f, posX, posY, width, iters, radius)
        = presets !! n

      render t x y w l r
        = A.map (escapeToRGBA l (A.use worldPalette))
        $ julia worldSizeX worldSizeY (\a b -> f (A.toFloating a) b) t x y w l r
  in
  world
    { worldRender = runN (get optBackend opts) render
    , worldPosX   = unit' posX
    , worldPosY   = unit' posY
    , worldWidth  = unit' width
    , worldIters  = unit' iters
    , worldRadius = unit' radius
    }

golden :: Floating a => a
golden = (1 + sqrt 5) / 2

presets
    :: (Fractional a, A.RealFloat a)
    => [ (Exp a -> Exp (Complex a) -> Exp (Complex a), a, a, a, Int32, a) ]
presets =
  [ (\t z -> z ^ (2::Int) + mkPolar 0.7885 t,                                   0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift ((1 - golden) * cos t :+ A.constant 0),      0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift ((golden-2) * cos t :+ (golden-1) * sin t),  0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift (0.285 * cos t :+ 0.1 * sin t),              0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift (0.45 * cos t :+ 0.1428 * sin t),            0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift ((-0.70176) * cos t :+ (-0.3842) * sin t),   0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift ((-0.835) * cos t :+ (-0.2321) * sin t),     0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift ((-0.8) * cos t :+ 0.156 * sin t),           0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift ((-0.7269) * cos t :+ 0.1889 * sin t),       0, 0, 4, 255, 16.0)
  , (\t z -> z ^ (2::Int) + A.lift (0 :+ (-0.8) * sin t),                       0, 0, 4, 255, 16.0)
  ]

