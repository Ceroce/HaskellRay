module Hitable (HitIntersection(..), HitResult(NoHit, FrontHit)) where

import Ray
import Vector

-- An intersection between the Ray and the hitable
data HitIntersection = HitIntersection { t :: Float -- Distance from the origin of the ray
                                       , point :: Vec3
                                       , normal :: Vec3} deriving (Show)

data HitResult = NoHit | FrontHit HitIntersection
    deriving Show

-- class Hitable a where
--     -- hitable, ray, (minT, maxT) -> HitResult
--     hitByRay :: a -> Ray -> (Float, Float) -> HitResult
