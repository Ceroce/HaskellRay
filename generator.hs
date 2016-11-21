-- Generates functions which describe list of random numbers.
-- The result should be piped to genLists.hs
-- This prevents using the random numbers generator inside haskellray.hs.

import System.Random

main = do
    putStrLn "-- This module was autogenerated by generator.hs. Don't edit."
    putStrLn "module Generated (jitter) where\n"
    g0 <- getStdGen
    g1 <- newStdGen
    let jitterCount = 100
    let jitterX = take jitterCount (randoms g0 :: [Float])
    let jitterY = take jitterCount (randoms g1 :: [Float])
    let jitter = zip jitterX jitterY
    putStrLn "jitter :: [(Float, Float)]"
    putStrLn $ "jitter = " ++ show jitter
    return ()
