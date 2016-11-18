module Camera (rayForUV) where

import Vector
import Ray

data Camera = Camera { position :: Vec3
                     , lowerLeft :: Vec3
                     , horizontal :: Vec3
                     , vertical :: Vec3 } deriving (Show)

defaultCamera :: Camera
defaultCamera = Camera { position = Vec3 0 0 0
                       , lowerLeft = Vec3 (-2) (-1) (-1)
                       , horizontal = Vec3 4 0 0
                       , vertical = Vec3 0 2 0 }

rayForUV :: Float -> Float -> Ray
rayForUV u v = rayForCameraUV defaultCamera u v

rayForCameraUV :: Camera -> Float -> Float -> Ray
rayForCameraUV cam u v = Ray { origin = position cam
                             , direction = (lowerLeft cam) + vec3FromFloat u * (horizontal cam) + vec3FromFloat v * (vertical cam) }
