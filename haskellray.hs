import Codec.Picture    -- Juicy.Pixels

renderingWidth = 200
renderingHeight = 100

pixelAtXY :: Int -> Int -> PixelRGB8
pixelAtXY x y = PixelRGB8  (floor (255.99 * r)) (floor (255.99 * g)) (floor (255.99 * b))
    where r = (fromIntegral x / fromIntegral renderingWidth)
          g = (fromIntegral y / fromIntegral renderingHeight)
          b = 0.2
main = do
    let path = "image.png"
    writePng path $ generateImage pixelAtXY renderingWidth renderingHeight
    putStrLn "Rendering finished"
    return ()
