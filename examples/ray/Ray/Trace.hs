{-# LANGUAGE ScopedTypeVariables #-}

module Ray.Trace
  where

-- friends
import Common.Type
import Scene.Object
import Scene.Light
import Ray.Intersect

-- frenemies
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Data.Colour.RGB                    as RGB
import Data.Array.Accelerate.Data.Colour.Names
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Vector
import Graphics.Gloss.Accelerate.Data.Point

-- standard library
import qualified Prelude                                        as P


-- | Generate all of the rays that will be cast from the given eye position to
--   cover the entire field of view.
--
castViewRays
    :: Int                              -- width of the display
    -> Int                              -- height
    -> Int                              -- field of view
    -> Exp Position                     -- eye position
    -> Acc (Array DIM2 Direction)       -- all rays originating from the eye position
castViewRays sizeX sizeY fov eyePos
  = let
        sizeX'          = P.fromIntegral sizeX
        sizeY'          = P.fromIntegral sizeY
        aspect          = sizeX' / sizeY'
        fov'            = P.fromIntegral fov
        fovX            = fov' * aspect
        fovY            = fov'
    in
    A.generate (constant (Z :. sizeY :. sizeX))
               (\ix -> let (x, y) = xyOfPoint $ pointOfIndex sizeX sizeY ix
                       in  normalize $ lift (V3 (x * fovX) ((-y) * fovY) 0) - eyePos)


-- | Cast a single ray into the scene
--
traceRay
    :: Int                              -- ^ Maximum reflection count
    -> Acc Objects                      -- ^ Objects in the scene
    -> Acc Lights                       -- ^ Direct lighting in the scene
    -> Exp Colour                       -- ^ Ambient light in the scene
    -> Exp Position                     -- ^ Origin of the ray
    -> Exp Direction                    -- ^ Direction of the ray
    -> Exp Colour
traceRay limit objects lights ambient = go limit
  where
    (spheres, planes)   = unlift objects

    dummySphere         = constant (Sphere_ (V3 0 0 0) 0          (RGB 0 0 0) 0)
    dummyPlane          = constant (Plane_  (V3 0 0 0) (V3 0 0 1) (RGB 0 0 0) 0)

    -- Stop once there are too many reflections, in case we've found two
    -- parallel mirrors.
    --
    go :: Int -> Exp Position -> Exp Direction -> Exp Colour
    go 0 _ _
      = constant black

    go bounces orig dir
      = let
            -- See which objects the ray intersects. Since we have no sum
            -- types, we need to do this separately for each object type,
            -- and determine the closest separately.
            --
            (hit_s, dist_s, s)  = unlift $ castRay distanceToSphere dummySphere spheres orig dir
            (hit_p, dist_p, p)  = unlift $ castRay distanceToPlane  dummyPlane  planes  orig dir
        in
        A.not (hit_s || hit_p) ?
          -- ray didn't intersect any objects
        ( constant black

          -- ray hit an object
        , let
              -- Determine the intersection point, and surface properties that
              -- will contribute to the colour
              next_s      = hitSphere     s dist_s orig dir
              next_p      = hitPlaneCheck p dist_p orig dir

              (point, normal, colour, shine)
                          = unlift (dist_s < dist_p ? ( next_s, next_p ))

              -- result angle of ray after reflection
              newdir      = dir - (2.0 * (normal `dot` dir)) *^ normal

              -- determine the direct lighting at this point
              direct      = applyLights objects lights point normal

              -- see if the ray hits anything else
              refl        = go (bounces - 1) point newdir

              -- total lighting is the direct lighting plus ambient
              lighting    = direct + ambient

              -- total incoming light is direct lighting plus reflections
              light_in    = scaleColour shine         refl
                          + scaleColour (1.0 - shine) lighting

              -- outgoing light is incoming light modified by surface colour. We
              -- also need to clip it in case the sum of all incoming lights is
              -- too bright to display.
              light_out   = RGB.clamp (light_in * colour)
          in
          light_out
        )


scaleColour :: Exp Float -> Exp Colour -> Exp Colour
scaleColour s c = lift (RGB s s s) * c

hitSphere :: Exp Sphere -> Exp Float -> Exp Position -> Exp Direction -> Exp (Position, Direction, Colour, Float)
hitSphere sph dist orig dir
  = let
        point   = orig + dist *^ dir
        normal  = sphereNormal sph point
        colour  = sphereColor sph
        shine   = sphereShine sph
    in
    lift (point, normal, colour, shine)

hitPlane :: Exp Plane -> Exp Float -> Exp Position -> Exp Direction -> Exp (Position, Direction, Colour, Float)
hitPlane pln dist orig dir
  = let
        point   = orig + dist *^ dir
        normal  = planeNormal pln
        colour  = planeColor pln
        shine   = planeShine pln
    in
    lift (point, normal, colour, shine)

hitPlaneCheck :: Exp PlaneCheck -> Exp Float -> Exp Position -> Exp Direction -> Exp (Position, Direction, Colour, Float)
hitPlaneCheck pln dist orig dir
  = let
        point   = orig + dist *^ dir
        normal  = planeCheckNormal pln
        colour  = checkers point
        shine   = planeCheckShine pln
    in
    lift (point, normal, colour, shine)

