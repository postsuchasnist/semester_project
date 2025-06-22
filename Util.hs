--util module, that stores some functions which were common for all programs

module Util(distance, Point(..), pretty, counterClockWise) where

import Text.Printf

--defining a type point which is a tuple of two doubles

type Point = (Double, Double)

--computing an Euclidean distance between two points

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt (f x1 x2 + f y1 y2)
    where f a b = (a - b) ^ 2

-- a function to make an output more readable

pretty :: [Point] -> String
pretty = unlines . map (\(x, y) -> "\t" ++ show x ++ " " ++ show y) 

-- a function that checks the convexity of three points

counterClockWise :: Num a => (a, a) -> (a, a) -> (a, a) -> a
counterClockWise (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)