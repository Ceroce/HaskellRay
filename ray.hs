module Ray (Ray (..),  pointAlongRay) where

import Vector

data Ray = Ray { origin :: Vec3
               , direction :: Vec3 } deriving (Show)

-- A ray is a parametric line: Point = origin + t*direction
pointAlongRay :: Ray -> Float -> Vec3
-- pointAlongRay ray t = Vec3 (x $ origin ray) (y $ origin ray) (z $ origin ray)
pointAlongRay ray t = origin ray + vec3FromFloat t * (direction ray)
