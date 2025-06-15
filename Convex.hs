module Convex (convex) where

import Text.Printf
import Data.List
import System.Environment (getArgs)
import System.Random 
import Generator
import Util

findMinYCoordinate :: [Point] -> Point
findMinYCoordinate (p : points) = f p points
    where 
        f prev [] = prev
        f p0@(x0, y0) (p1@(x1, y1) : points) | y1 < y0 = f p1 points
                                             | (y1 == y0) && (x1 < x0) = f p1 points
                                             | otherwise = f p0 points

filterSameAngle :: [(Point, Double)] -> [Point]
filterSameAngle lst = map fst . filter snd $ r
    where
        r = zipWith (\(pa, ra) (pb, rb) -> (pa, ra /= rb)) lst ((drop 1 lst) ++ [head lst])

build :: [Point] -> [Point] -> [Point]
build hull [] = hull
build hs@(h1:[]) ps@(p:points) = build (p : h1 : []) points
build hs@(h2 : h1 : hull) ps@(p : points) = hull'
    where 
        rightTurn = counterClockWise h1 h2 p < 0
        collinear = counterClockWise h2 h1 p == 0
        hull' | rightTurn = build (h1 : hull) ps
              | collinear = build (p : h1 : hull) points
              | otherwise = build (p:hs) points

convexHull :: [(Double, Double)] -> [(Double, Double)]
convexHull points = hull
    where 
        p0@(x0, y0) = findMinYCoordinate points
        sorted' = let
                    o (a, ra) (b, rb) | ra > rb = GT
                                      | (ra == rb) && ((distance a p0) > (distance b p0)) = GT
                                      | otherwise = LT 
                in sortBy o hullP

        f(x, y) = r'
            where r = atan $ (y - y0) / (x - x0)
                  r' | r < 0 = r + 2 * pi
                     | otherwise = r
        hullP = map (\p -> (p, f p)) $ delete p0 points 

        sorted = filterSameAngle sorted'

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
                    