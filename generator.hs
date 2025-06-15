module Generator (generate) where 

import System.Random 

type Point = (Double, Double)

generate :: Int -> IO [Point]
generate n = sequence [ (,) <$> r <*> r | _ <- [1..n] ]
    where r = randomRIO(-100, 100 :: Double)