import Codec.Picture    -- Juicy.Pixels
import System.Random as Rand
import Vector
import Ray
import Sphere
import Hitable
import Camera

renderingWidth = 200
renderingHeight = 100

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

colorForRayAndSpheres :: Ray -> [Sphere] -> Vec3
colorForRayAndSpheres ray [] = backgroundColorForRay ray
colorForRayAndSpheres ray (sph:sphs) = case result of
    FrontHit intersection -> (vec3FromFloat 0.5) * (normal intersection + vec3FromFloat 1.0)
    NoHit -> colorForRayAndSpheres ray sphs
    where result = hitByRay sph ray rayRange

rayForXY :: (Float, Float) -> Ray
rayForXY (x, y) = rayForUV u v
    where u = x / fromIntegral renderingWidth
          v = 1.0 - y / fromIntegral renderingHeight

colorAtXY :: (Float, Float) -> Vec3
colorAtXY coord = colorForRayAndSpheres (rayForXY coord) spheres

-- Generate pairs of pseudo-random numbers between 0 and 1
jitter :: Int -> [(Float, Float)]
jitter count = zip x y
    where g0 = Rand.mkStdGen 0
          g1 = Rand.mkStdGen 1000000 -- Take a seed big enough so g0's serie does not overlap
          x = take count (randoms g0 :: [Float])
          y = take count (randoms g1 :: [Float])

supersampledColorAtXY :: (Float, Float) -> Vec3
supersampledColorAtXY (x, y) = colorSum / (fromIntegral rayCount)
    where rayCount = 100
          jitteredCoords = map (\(jX, jY) -> (x+jX, y+jY)) $ jitter rayCount
          colorSum = foldl (+) (vec3FromFloat 0.0) (map colorAtXY jitteredCoords)

pixelAtXY :: Int -> Int -> PixelRGB8
pixelAtXY x y = PixelRGB8  (floor (255.99 * r)) (floor (255.99 * g)) (floor (255.99 * b))
    where (Vec3 r g b) = supersampledColorAtXY (fromIntegral x, fromIntegral y)

main = do
    let path = "image.png"
    writePng path $ generateImage pixelAtXY renderingWidth renderingHeight
    putStrLn "Rendering finished"
    return ()
