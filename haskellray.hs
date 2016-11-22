import Codec.Picture    -- Juicy.Pixels
import System.Random as Rand
import Vector
import Ray
import Sphere
import Hitable
import Camera

renderingWidth = 200
renderingHeight = 100
rayCount = 10

-- For whatever reason, collisions with the ray are limited to a range of values of t
-- I might remove it later if it does not prove to be useful
rayRange = (0, 1000000)

backgroundColorForRay :: Ray -> Vec3
backgroundColorForRay ray = mix white blue t
    where normDir = normalize $ direction ray
          t = 0.5 * (y normDir + 1.0)
          white = Vec3 1.0 1.0 1.0
          blue = Vec3 0.5 0.7 1.0

spheres :: [Sphere]
spheres = [ Sphere { center = Vec3 0.0 0.0 (-1), radius = 0.5}
          , Sphere { center = Vec3 0.0 (-100.5)(-1), radius = 100}]

-- float between 0 and 1
randomFloat :: StdGen -> (Float, StdGen)
randomFloat g = random g :: (Float, StdGen)

-- Coordinates between -1 and 1
randomVec3 :: StdGen -> (Vec3, StdGen)
randomVec3 g = (Vec3 x' y' z', gz)
    where (x, gx) = randomFloat g
          (y, gy) = randomFloat gx
          (z, gz) = randomFloat gy
          x' = -1 + 2 * x
          y' = -1 + 2 * y
          z' = -1 + 2 * z

randomPointInsideUnitSphere :: StdGen -> (Vec3, StdGen)
randomPointInsideUnitSphere g = (vec3FromFloat radius * (normalize v), gr)
    where (v, gv) = randomVec3 g
          (radius, gr) = randomFloat gv

-- This does not seem to work correctly. I suspect that unitSpPoint is always the same.
-- I would say this function is OK, but it is not passed the right number generator.
newRayAtIntersection :: HitIntersection -> StdGen -> (Ray, StdGen)
newRayAtIntersection inter g = (Ray { origin = point inter, direction = target - point inter}, gSph)
    where (unitSphPoint, gSph) = randomPointInsideUnitSphere g
          target = point inter + normal inter + unitSphPoint

closestHitAmongSpheres :: Ray -> [Sphere] -> HitResult
closestHitAmongSpheres _ [] = NoHit
closestHitAmongSpheres ray (sph:sphs) = if isHitResultCloser left right then left else right
    where left = hitByRay sph ray rayRange
          right = closestHitAmongSpheres ray sphs

colorForRayAndSpheres :: Ray -> [Sphere] -> StdGen -> Vec3
colorForRayAndSpheres ray spheres gen = let result = closestHitAmongSpheres ray spheres
    in case result of
    FrontHit intersection -> diffuse * colorForRayAndSpheres newRay spheres newGen
        where diffuse = vec3FromFloat 0.5
              (newRay, newGen) = newRayAtIntersection intersection gen
    NoHit -> backgroundColorForRay ray

rayForXY :: (Float, Float) -> Ray
rayForXY (x, y) = rayForUV u v
    where u = x / fromIntegral renderingWidth
          v = 1.0 - y / fromIntegral renderingHeight

colorAtXY :: (Float, Float) -> Vec3
colorAtXY coord = colorForRayAndSpheres (rayForXY coord) spheres (mkStdGen 0)

-- Generate pairs of pseudo-random numbers between 0 and 1
jitter :: Int -> [(Float, Float)]
jitter count = zip x y
    where g0 = Rand.mkStdGen 0
          g1 = Rand.mkStdGen 1000000 -- Take a seed big enough so g0's serie does not overlap
          x = take count (randoms g0 :: [Float])
          y = take count (randoms g1 :: [Float])

supersampledColorAtXY :: (Float, Float) -> Vec3
supersampledColorAtXY (x, y) = colorSum / (fromIntegral rayCount)
    where jitteredCoords = map (\(jX, jY) -> (x+jX, y+jY)) $ jitter rayCount
          colorSum = foldl (+) (vec3FromFloat 0.0) (map colorAtXY jitteredCoords)

pixelAtXY :: Int -> Int -> PixelRGB8
pixelAtXY x y = PixelRGB8  (floor (255.99 * r)) (floor (255.99 * g)) (floor (255.99 * b))
    where (Vec3 r g b) = supersampledColorAtXY (fromIntegral x, fromIntegral y)

main = do
    let path = "image.png"
    writePng path $ generateImage pixelAtXY renderingWidth renderingHeight
    putStrLn "Rendering finished"
    return ()
