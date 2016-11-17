import Codec.Picture    -- Juicy.Pixels
import Vector

renderingWidth = 200
renderingHeight = 100

pixelAtXY :: Int -> Int -> PixelRGB8
pixelAtXY x y = PixelRGB8  (floor (255.99 * r)) (floor (255.99 * g)) (floor (255.99 * b))
    where r = (fromIntegral x / fromIntegral renderingWidth)
          g = (fromIntegral y / fromIntegral renderingHeight)
          b = 0.2
main = do
    let path = "image.png"
    let v = Vec3 1 2 3
    let l = len v
    putStrLn $ "Vector: " ++ show v ++ "length: " ++ show l
    writePng path $ generateImage pixelAtXY renderingWidth renderingHeight
    putStrLn "Rendering finished"
    return ()
