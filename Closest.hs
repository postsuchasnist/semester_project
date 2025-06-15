-- a module responsible for finding the closest pair of points

module Closest (closest) where

import Data.List (sortBy, minimumBy)
import Data.Function (on)
import Generator
import Util

-- we make this type for convenience of handling the input with distance and points forming this distance

type Result = (Double, (Point, Point))

-- brute-force for small inputs (n <= 3), it can be done very fast
bruteForce :: [Point] -> Result
bruteForce pts = minimumBy (compare `on` fst)
  [ (distance p q, (p, q)) | (p : qs) <- tails pts, q <- qs ]
  where
    tails [] = []
    tails xs@(_ : rest) = xs : tails rest

-- Strip closest: find best pair in vertical strip
stripClosest :: [Point] -> Double -> Result -> Result
stripClosest strip d best = go strip best
  where
    go [] res = res
    go (p : ps) res@(minDist , _) = go ps (checkNext p ps minDist res)

    checkNext _ [] _ res = res
    checkNext p (q : qs) d res
      | snd q - snd p < d =
          let dist = distance p q
          in checkNext p qs d (if dist < fst res then (dist, (p, q)) else res)
      | otherwise = res

-- divide and conquer algorithm to calculate it
closestUtil :: [Point] -> [Point] -> Result
closestUtil px py
  | length px <= 3 = bruteForce px
  | otherwise =
      let mid = length px `div` 2
          midPoint = px !! mid
          (lx, rx) = splitAt mid px
          (ly, ry) = partitionByX midPoint py
          leftRes = closestUtil lx ly
          rightRes = closestUtil rx ry
          best@(d, _) = minimumBy (compare `on` fst) [leftRes, rightRes]
          strip = filter (\p -> abs (fst p - fst midPoint) < d) py
      in stripClosest strip d best

--split py into left and right according to mid x
partitionByX :: Point -> [Point] -> ([Point], [Point])
partitionByX midPoint = foldr split ([], [])
  where
    split p (l, r)
      | fst p <= fst midPoint = (p:l, r)
      | otherwise         = (l, p:r)

--getting a closest pair
closestPair :: [Point] -> Result
closestPair pts =
  let px = sortBy (compare `on` fst) pts
      py = sortBy (compare `on` snd) pts
  in closestUtil px py

--function that transforms Result into string
formatResult :: Result -> String
formatResult (dist, (p1, p2)) =
  "The smallest distance is " ++ show dist ++ "\n" ++
  "Between points: " ++ showPoint p1 ++ " and " ++ showPoint p2

--function to show a point approprietly 
showPoint :: Point -> String
showPoint (x, y) = "Point {x = " ++ show x ++ ", y = " ++ show y ++ "}"

closest :: Int -> IO ()
closest n
    | n <= 2 = putStrLn "Please enter an integer greater than 2"
    | otherwise = do
        points <- generate n
        putStrLn "Generated Points:"
        putStrLn $ pretty points
        putStrLn "Closest:"
        let result = closestPair points
        putStrLn $ formatResult result