module Sphere (Sphere (..), hitByRay) where

import Hitable
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
hitByRay :: Sphere -> Ray -> (Float, Float) -> HitResult
hitByRay sphere ray range =
    if discr > 0
    then    if inRange rootClose range
            then FrontHit $ intersection ray rootClose sphere
            else    if inRange rootFar range
                    then FrontHit $ intersection ray rootFar sphere
                    else NoHit
    else NoHit
    where   oc = origin ray - center sphere
            sphRad = radius sphere
            a = dot (direction ray) (direction ray)
            b = 2 * dot oc (direction ray)
            c = dot oc oc - sphRad * sphRad
            discr = b * b - 4 * a * c
            rootClose = (-b - sqrt discr) / (2 * a)
            rootFar   = (-b + sqrt discr) / (2 * a)

intersection :: Ray -> Float -> Sphere -> HitIntersection
intersection ray root sphere = HitIntersection root point normal
    where   point = pointAlongRay ray root
            normal = (point - center sphere) / vec3FromFloat (radius sphere)

inRange :: Float -> (Float, Float) -> Bool
inRange t (tMin, tMax) = (tMin < t) && (t < tMax)
