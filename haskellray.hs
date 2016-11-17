import Codec.Picture    -- Juicy.Pixels
import Vector

renderingWidth = 200
renderingHeight = 100

colorAtXY :: Int -> Int -> Vec3
colorAtXY x y = Vec3 r g b
    where r = fromIntegral x / fromIntegral renderingWidth
          g = fromIntegral y / fromIntegral renderingHeight
          b = 0.2

pixelAtXY :: Int -> Int -> PixelRGB8
pixelAtXY x y = PixelRGB8  (floor (255.99 * r)) (floor (255.99 * g)) (floor (255.99 * b))
    where (Vec3 r g b) = colorAtXY x y

main = do
    let path = "image.png"
    writePng path $ generateImage pixelAtXY renderingWidth renderingHeight
    putStrLn "Rendering finished"
    return ()
