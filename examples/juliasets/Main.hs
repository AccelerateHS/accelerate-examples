{-# LANGUAGE RecordWildCards #-}

import           Data.Array.Accelerate.Examples.Internal as A

import           System.Exit

import           Data.Array.Accelerate                   (Array, DIM2, Elt,
                                                          Scalar, Word32)
import           Data.Array.Accelerate                   as A hiding (unit)
import           Graphics.Gloss.Accelerate.Data.Picture  as A

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game        as G

import           Prelude                                 as P

import           Data.Label

import           Config
import           IterFuns
import           Lib
import           World

-- main :: IO ()
-- main = do
--   beginMonitoring
--   (conf, opts, rest)    <- parseArgs options defaults header footer
--   putStrLn "Hello wolrd"
--   return ()

main :: IO ()
main = do
  beginMonitoring
  (conf, opts, _) <- parseArgs options defaults header footer

  let world  = starterWorld conf
      -- world  = initialWorld conf opts
      width  = get configWidth conf   :: Int
      height = get configHeight conf  :: Int

  if (width P.== height)
    then G.playIO (InWindow "Julia set simulation" (width, width) (0, 0))
                  black
                  60
                  world
                  (makePicture (get optBackend opts))
                  eventHandler
                  timeHandler
    else putStrLn "Non-square window not supported at this time"

starterWorld :: Config -> World
starterWorld config = World { dimens = get configWidth config
                            , time = 0
                            , zoom = 1
                            , speed = 0.2 --1 is already very fast
                            , simState = Normal
                            , offset = (0,0)
                            }

eventHandler :: Event -> World -> IO World
eventHandler e w@World{..} = case e of
  EventKey (Char c) Down _ _ -> case c of
    'w' -> return w{zoom = zoom * 1.25} -- zoom in  by 25%
    's' -> return w{zoom = zoom * 0.8 } -- zoom out by 25%


    'a' -> return w{speed = speed * 0.8 } -- slow  down by 25%
    'd' -> return w{speed = speed * 1.25} -- speed up   by 25%

    _   -> return w

  EventKey (SpecialKey KeyEsc)   Down _ _ -> exitSuccess
  EventKey (SpecialKey KeySpace) Down _ _ -> case simState of
    Normal -> return w{simState = Paused}
    Paused -> return w{simState = Normal}

  EventKey (SpecialKey KeyUp) Down _ _ ->
    return w{offset = (x, y + adjust)}
  EventKey (SpecialKey KeyDown) Down _ _ ->
    return w{offset = (x, y - adjust)}
  EventKey (SpecialKey KeyRight) Down _ _ ->
    return w{offset = (x + adjust, y)}
  EventKey (SpecialKey KeyLeft) Down _ _ ->
    return w{offset = (x - adjust, y)}

  _ -> return w

  where
    adjust = 0.1 * (P.fromIntegral dimens / zoom)
    (x,y)  = offset

timeHandler :: Float -> World -> IO World
timeHandler delta w@World{..} = case simState of
  Normal -> return $ w{time = time + (delta * speed)}
  Paused -> return w

makePicture :: Backend -> World -> IO Picture
makePicture backend World{..} = return $ A.bitmapOfArray arr False
  where
    arr = juliaArray backend
                     dimens
                     (unit time)
                     (unit zoom)
                     (unit offset)

unit :: Elt a => a -> Scalar a
unit a = A.fromList A.Z [a]

-- -- juliaArray :: Int
-- -- juliaArray :: Array DIM2 Word32
-- juliaArray :: Int
--            -> Scalar Float -- time
--            -> Scalar Float -- zoom
--            -> Scalar (Float, Float) -- zoom
--            -> Array DIM2 Word32
-- juliaArray dimens = undefined

juliaArray :: Backend
           -> Int
           -> Scalar Float -- time
           -> Scalar Float -- zoom
           -> Scalar (Float, Float) -- offset
           -> Array DIM2 Word32
juliaArray backend dimens time zoom offset = (run1 backend) uncurried (time, zoom, offset)
  where
    uncurried :: Acc (Scalar Float, Scalar Float, Scalar (Float, Float)) -> Acc (Array DIM2 Word32)
    uncurried = uncurry3 (runJulia (iterQuad 0.7885) dimens)
