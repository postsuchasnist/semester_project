-- module that is responsible for computing a convex hull of points

module Convex (convex) where

import Text.Printf
import Data.List
import System.Environment (getArgs)
import System.Random 
import Generator
import Util
import Data.Ord
import Data.Tuple

-- function to find a minimum y coordinate among points

findMinYCoordinate :: [Point] -> Point
findMinYCoordinate ps = minimumBy (comparing swap) ps

-- filtering out cnsecutive points with the same angle

filterSameAngle :: [(Point, Double)] -> [Point]
filterSameAngle lst = map fst . filter snd $ r
    where
        r = zipWith (\(pa, ra) (pb, rb) -> (pa, ra /= rb)) lst ((drop 1 lst) ++ [head lst])

-- building a hull

build :: [Point] -> [Point] -> [Point]
build hull [] = hull
build hs@(h1 : []) ps@(p : points) = build (p : h1 : []) points
build hs@(h2 : h1 : hull) ps@(p : points) = hull'
    where 
        rightTurn = counterClockWise h1 h2 p < 0
        collinear = counterClockWise h2 h1 p == 0
        hull' | rightTurn = build (h1 : hull) ps
              | collinear = build (p : h1 : hull) points
              | otherwise = build (p:hs) points

-- finding the convex hull

convexHull :: [(Double, Double)] -> [(Double, Double)]
convexHull points = hull
    where 

        -- find the point which is on the bottom
        p0@(x0, y0) = findMinYCoordinate points

        --sort in increasing order of the angle points with the bottom point make with x-axis
        sorted' = sortOn (\(a, ra) -> (ra, distance a p0)) hullP

        f(x, y) = r'
            where r = atan $ (y - y0) / (x - x0)
                  r' | r < 0 = r + 2 * pi
                     | otherwise = r
        hullP = map (\p -> (p, f p)) $ delete p0 points 

        sorted = filterSameAngle sorted'

        --construct a convex hull
        hull = build [p0] sorted 

convex :: Int -> IO ()
convex n
    | n <= 2 = putStrLn "Please enter an integer greater than 2"
    | otherwise = do
        points <- generate n
        putStrLn "Generated Points:"
        putStrLn $ pretty points
        putStrLn "Convex Hull:"
        putStrLn $ pretty (convexHull points)         
                    