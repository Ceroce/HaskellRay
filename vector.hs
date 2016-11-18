module Vector (Vec3 (..), vec3FromFloat, len, normalize, mix) where

data Vec3 = Vec3 { x :: Float
                 , y :: Float
                 , z :: Float } deriving (Show)

-- Since Vec3 inherits the Num class, all operators of Num must be defined
instance Num Vec3 where
    Vec3 x0 y0 z0 + Vec3 x1 y1 z1 = Vec3 (x0 + x1) (y0 + y1) (z0 + z1)
    Vec3 x0 y0 z0 - Vec3 x1 y1 z1 = Vec3 (x0 - x1) (y0 - y1) (z0 - z1)
    Vec3 x0 y0 z0 * Vec3 x1 y1 z1 = Vec3 (x0 * x1) (y0 * y1) (z0 * z1)
    abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
    signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum y)
    fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

-- Also define Vec3 as inheriting Factional, so it can be divided
instance Fractional Vec3 where
    Vec3 x0 y0 z0 / Vec3 x1 y1 z1 = Vec3 (x0 / x1) (y0 / y1) (z0 / z1)
    fromRational r = Vec3 (fromRational r) (fromRational r) (fromRational r)

vec3FromFloat :: Float -> Vec3
vec3FromFloat x = Vec3 x x x

len :: Vec3 -> Float
len v = sqrt $ squaredLen v

squaredLen :: Vec3 -> Float
squaredLen (Vec3 x y z) = x * x + y * y + z * z

normalize :: Vec3 -> Vec3
normalize v = v / (vec3FromFloat $ len v)

dot :: Vec3 -> Vec3 -> Vec3
dot (Vec3 x0 y0 z0) (Vec3 x1 y1 z1)
    = Vec3 (x0*y1 - x1*y0) (y0*z1 - y1*z0) (z0*x1 - z1*x0)

-- Linear interpolation
mix :: Vec3 -> Vec3 -> Float -> Vec3
mix u v a = (vec3FromFloat (1.0 - a)) * u + (vec3FromFloat a) * v
