module Sphere (Sphere (..), isSphereHitByRay, rootSphereHitByRay) where

import Vector
import Ray

data Sphere = Sphere { center :: Vec3
                     , radius :: Float } deriving (Show)

{- Injecting the equation of a ray into the equation of a sphere
provides a 2nd order equation:
  t^2*dot(B, B) + 2*t*dot(B, A-C) + dot(A-C, A-C) - R^2 = 0
if discriminant == 0 the sphere is hit once (border of the sphere),
if discriminant > 0, the sphere is hit twice (front and back)
-}
isSphereHitByRay :: Sphere -> Ray -> Bool
isSphereHitByRay sphere ray = rootSphereHitByRay sphere ray > 0.0

-- We only consider a single root: the one closer to the camera
-- The root is the value of t in the Ray's equation.
rootSphereHitByRay :: Sphere -> Ray -> Float
rootSphereHitByRay sphere ray = if discr > 0 then (-b - sqrt discr) / (2.0 * a) else -1.0
    where   oc = origin ray - center sphere
            sphRad = radius sphere
            a = dot (direction ray) (direction ray)
            b = 2.0 * dot oc (direction ray)
            c = dot oc oc - sphRad * sphRad
            discr = b * b - 4.0 * a * c
