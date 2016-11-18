import Codec.Picture    -- Juicy.Pixels
import Vector
import Ray
import Sphere

renderingWidth = 200
renderingHeight = 100


backgroundColorForRay :: Ray -> Vec3
backgroundColorForRay ray = mix white blue t
    where normDir = normalize $ direction ray
          t = 0.5 * (y normDir + 1.0)
          white = Vec3 1.0 1.0 1.0
          blue = Vec3 0.5 0.7 1.0

colorForRay :: Ray -> Vec3
colorForRay ray = if root > 0.0 then (vec3FromFloat 0.5) * (normal + vec3FromFloat 1.0)
    else backgroundColorForRay ray
    where   sphere = Sphere { center = Vec3 0.0 0.0 (-1.0), radius = 0.5}
            root = rootSphereHitByRay sphere ray
            hitPoint = pointAlongRay ray root
            normal = normalize (hitPoint - center sphere)

rayForXY :: Int -> Int -> Ray
rayForXY x y = Ray { origin = Vec3 0.0 0.0 0.0
                   , direction = lowerLeft + vec3FromFloat u * horiz + vec3FromFloat v * vert }
    where lowerLeft = Vec3 (-2.0) (-1.0) (-1.0)
          horiz = Vec3 4.0 0.0 0.0
          vert  = Vec3 0.0 2.0 0.0
          u = fromIntegral x / fromIntegral renderingWidth
          v = 1.0 - fromIntegral y / fromIntegral renderingHeight

colorAtXY :: Int -> Int -> Vec3
colorAtXY x y = colorForRay $ rayForXY x y

pixelAtXY :: Int -> Int -> PixelRGB8
pixelAtXY x y = PixelRGB8  (floor (255.99 * r)) (floor (255.99 * g)) (floor (255.99 * b))
    where (Vec3 r g b) = colorAtXY x y

main = do
    let path = "image.png"
    writePng path $ generateImage pixelAtXY renderingWidth renderingHeight
    putStrLn "Rendering finished"
    return ()
