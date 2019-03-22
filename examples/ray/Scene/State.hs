{-# LANGUAGE TemplateHaskell #-}

module Scene.State
  where

-- friends
import Common.Type
import Scene.Light
import Scene.Object
import Scene.World

-- frenemies
import Data.Array.Accelerate                                    as A

-- library
import Prelude                                                  hiding ( (.), id )
import Data.Label
import Control.Category
import qualified Graphics.Gloss                                 as G


data Move  = Fwd | Rev
  deriving Show

data State = State
  { _stateTime                  :: !Float
  , _stateEyePos                :: !Position
  , _stateEyeDelta              :: !Position
  , _stateLightDelta            :: !Position

  , _stateLeftClick             :: !(Maybe G.Point)

  , _stateMoveSpeed             :: !Float
  , _stateEyeHoriz              :: !(Maybe Move)
  , _stateEyeVert               :: !(Maybe Move)
  , _stateLightHoriz            :: !(Maybe Move)
  , _stateLightVert             :: !(Maybe Move)

  , _stateObjects               :: !Objects
  , _stateLights                :: !Lights
  }
  deriving Show

mkLabels [''State]


-- | Initialise the world and interface state
--
initState :: Float -> State
initState time
  = advanceState 0
  $ State
      { _stateTime              = time
      , _stateEyePos            = V3 50    (-100) (-700)

      , _stateEyeDelta          = V3 (-50) 200   1296
      , _stateLightDelta        = V3 0     0     0

      , _stateLeftClick         = Nothing

      , _stateMoveSpeed         = 400
      , _stateEyeHoriz          = Nothing
      , _stateEyeVert           = Nothing
      , _stateLightHoriz        = Nothing
      , _stateLightVert         = Nothing

      , _stateObjects           = makeObjects time
      , _stateLights            = makeLights  time
      }


-- | Advance the world forward in time
--
advanceState :: Float -> State -> State
advanceState dt state
  = setTime (get stateTime state + dt)
  $ move stateLightDelta stateLightVert  zz
  $ move stateLightDelta stateLightHoriz xx
  $ move stateEyeDelta stateEyeVert  zz
  $ move stateEyeDelta stateEyeHoriz xx
  $ state
  where
    speed        = get stateMoveSpeed state
    move eye f d = case get f state of
                    Nothing     -> id
                    Just Fwd    -> modify eye (+ (set d ( speed * dt) (V3 0 0 0)))
                    Just Rev    -> modify eye (+ (set d (-speed * dt) (V3 0 0 0)))

    zz          = lens (\(V3 _ _ z) -> z) (\f (V3 x y z) -> V3 x y (f z))
    xx          = lens (\(V3 x _ _) -> x) (\f (V3 x y z) -> V3 (f x) y z)


-- | Set the time of the world
--
setTime :: Float -> State -> State
setTime time state
  = let
        objects         = makeObjects time
        lights          = makeLights  time
        deltaEye        = get stateEyeDelta   state
        deltaLight      = get stateLightDelta state + deltaEye
    in
    set stateTime time
      $ set stateObjects (translateObjects deltaEye   objects)
      $ set stateLights  (translateLights  deltaLight lights)
      $ state


translateObjects :: Position -> Objects -> Objects
translateObjects v (spheres, planes)
  = ( fromList (arrayShape spheres) [ Sphere_ (p + v) r c s | Sphere_ p r c s <- toList spheres ]
    , fromList (arrayShape planes)  [ Plane_  (p + v) d c s | Plane_  p d c s <- toList planes  ]
    )

translateLights :: Position -> Lights -> Lights
translateLights v lights
  = fromList (arrayShape lights) [ Light_ (p + v) c | Light_ p c <- toList lights ]

