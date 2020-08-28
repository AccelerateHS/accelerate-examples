{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

module Scene.Object
  where

-- friends
import Common.Type

-- frenemies
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Data.Colour.RGB
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.Vector

-- standard library
import qualified Prelude                                        as P


-- | All objects in the scene
--
type Objects = (Vector Sphere, Vector Plane)

-- | Objects in the world. Accelerate does not have sum types, so define each
--   object separately (and hope this works out...)
--
data Sphere = Sphere_ Position Float Colour Float
  deriving (Show, Generic, Elt)

data Plane = Plane_ Position Direction Colour Float
  deriving (Show, Generic, Elt)

pattern Sphere :: Exp Position -> Exp Float -> Exp Colour -> Exp Float -> Exp Sphere
pattern Sphere { spherePos, sphereRadius, sphereColor, sphereShine } =
  Pattern ( spherePos, sphereRadius, sphereColor, sphereShine )
{-# COMPLETE Sphere #-}

pattern Plane :: Exp Position -> Exp Direction -> Exp Colour -> Exp Float -> Exp Plane
pattern Plane { planePos, planeNormal, planeColor, planeShine } =
  Pattern ( planePos, planeNormal, planeColor, planeShine )
{-# COMPLETE Plane #-}

type PlaneCheck = Plane

planeCheckPos    :: Exp PlaneCheck -> Exp Position
planeCheckNormal :: Exp PlaneCheck -> Exp Direction
planeCheckShine  :: Exp PlaneCheck -> Exp Float

planeCheckPos    = planePos
planeCheckNormal = planeNormal
planeCheckShine  = planeShine


-- | Compute the distance to the surface of a sphere
--
distanceToSphere
    :: Exp Sphere               -- ^ Object to intersect
    -> Exp Position             -- ^ Ray cast from this point...
    -> Exp Direction            -- ^ ...along this direction
    -> Exp (Bool, Float)        -- ^ Distance to intersection, if there is one
distanceToSphere sphere origin direction
  = let
        pos     = spherePos sphere
        radius  = sphereRadius sphere

        p       = origin + ((pos - origin) `dot` direction) *^ direction
        d_cp    = norm (p - pos)
        sep     = p - origin
        miss    = d_cp >= radius || sep `dot` direction <= 0
    in
    miss ? ( lift (False, infinity)
           , lift (True,  norm sep - sqrt (radius * radius - d_cp * d_cp)) )


-- | Compute the distance to the surface of a Plane
--
distanceToPlane
    :: Exp Plane                -- ^ Plane to intersect
    -> Exp Position             -- ^ Ray cast from this point
    -> Exp Direction            -- ^ ...along this direction
    -> Exp (Bool, Float)        -- ^ Distance to intersection, if there is one
distanceToPlane plane origin direction
  = let
        pos             = planePos plane
        normal          = planeNormal plane
        theta           = direction `dot` normal        -- TLM: name?
    in
    theta >= 0 ? ( lift (False, infinity)
                 , lift (True,  ((pos - origin) `dot` normal) / theta) )


-- | The maximum representable floating point value
--
infinity :: Exp Float
infinity = constant (P.encodeFloat m n)
  where
    a           = undefined :: Float
    b           = P.floatRadix a
    e           = P.floatDigits a
    (_, e')     = P.floatRange a
    m           = b P.^ e - 1
    n           = e' - e


-- | Compute the surface normal of a sphere at a point
--
sphereNormal
    :: Exp Sphere
    -> Exp Position             -- ^ A point on the surface of the sphere
    -> Exp Direction            -- ^ Normal at that point
sphereNormal sphere point
  = normalize (point - spherePos sphere)


-- | A checkerboard pattern along the x/z axis
--
checkers :: Exp Position -> Exp Colour
checkers pos
  = let
        x       = pos ^. _x
        z       = pos ^. _z
        v1      = (A.truncate (x / 100) :: Exp Int32) `mod` 2
        v2      = (A.truncate (z / 100) :: Exp Int32) `mod` 2
        v3      = A.fromIntegral . boolToInt $ x A.< 0.0
        v4      = A.fromIntegral . boolToInt $ z A.< 0.0
    in
    v1 `xor` v2 `xor` v3 `xor` v4 == 1 {- True -}
      ? ( rgb 1.0 1.0 1.0
        , rgb 0.4 0.4 0.4 )

