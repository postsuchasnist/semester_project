module Intersect (intersect) where

import Data.List (sortBy)
import qualified Data.Set as Set
import Data.Ord (comparing)
import Data.Array
import Util
import Generator
import Text.Printf

type Segment = (Point, Point)

data Event = Event
  { ex :: Double
  , ey :: Double
  , isLeft :: Bool
  , segIdx :: Int
  } deriving (Eq, Show)

instance Ord Event where
  compare = comparing (\(Event x y l _) -> (x, y, not l))

onSegment :: Point -> Point -> Point -> Bool
onSegment (px, py) (qx, qy) (rx, ry) =
  qx <= max px rx && qx >= min px rx &&
  qy <= max py ry && qy >= min py ry

orientation :: Point -> Point -> Point -> Int
orientation (px, py) (qx, qy) (rx, ry) =
  let val = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
  in if abs val < 1e-9 then 0 else if val > 0 then 1 else 2

doIntersect :: Segment -> Segment -> Bool
doIntersect (p1, q1) (p2, q2) =
  let o1 = orientation p1 q1 p2
      o2 = orientation p1 q1 q2
      o3 = orientation p2 q2 p1
      o4 = orientation p2 q2 q1
  in  o1 /= o2 && o3 /= o4 ||
      o1 == 0 && onSegment p1 p2 q1 ||
      o2 == 0 && onSegment p1 q2 q1 ||
      o3 == 0 && onSegment p2 p1 q2 ||
      o4 == 0 && onSegment p2 q1 q2

intersectPoint :: Segment -> Segment -> Maybe Point
intersectPoint ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  let denom = (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
  in if abs denom < 1e-9
     then Nothing
     else
       let x = ((x1*y2 - y1*x2)*(x3 - x4) - (x1 - x2)*(x3*y4 - y3*x4)) / denom
           y = ((x1*y2 - y1*x2)*(y3 - y4) - (y1 - y2)*(x3*y4 - y3*x4)) / denom
       in Just (x, y)

-- Sweep line intersection algorithm using an array for O(1) segment lookup
isIntersect :: Array Int Segment -> IO Int
isIntersect segments = go sortedEvents Set.empty Set.empty
  where
    -- Generate events from segments stored in array
    n = snd $ bounds segments
    events = concatMap mkEvents [0..n]
    mkEvents i =
      let seg@((x1,y1),(x2,y2)) = segments ! i
          (left@(lx, ly), right@(rx, ry)) = if x1 < x2 || (x1 == x2 && y1 < y2)
                                            then ((x1,y1),(x2,y2))
                                            else ((x2,y2),(x1,y1))
      in [Event lx ly True i, Event rx ry False i]

    sortedEvents = sortBy (comparing ex) events

    go [] _ reported = do
      putStrLn "Intersecting Lines:"
      mapM_ printIntersect (Set.toList reported)
      return $ Set.size reported

    go (e:es) active reported =
      let i = segIdx e
          seg = segments ! i
          activeList = Set.toAscList active
          prev = Set.lookupLT e active
          next = Set.lookupGT e active
          tryInsert a b s =
            if doIntersect (segments ! (segIdx a)) (segments ! (segIdx b))
              then Set.insert (min (segIdx a) (segIdx b), max (segIdx a) (segIdx b)) s
              else s
      in if isLeft e
         then
           let active' = Set.insert e active
               reported' = foldr (\x acc -> tryInsert x e acc) reported (filter (/= e) activeList)
           in go es active' reported'
         else
           let active' = Set.delete (Event (ex e) (ey e) True i) active
               reported' = case (prev, next) of
                             (Just p, Just n) -> tryInsert p n reported
                             _ -> reported
           in go es active' reported'

    printIntersect (i,j) = do
      let s1 = segments ! i
          s2 = segments ! j
      case intersectPoint s1 s2 of
        Just pt -> putStrLn $ show s1 ++ " intersects " ++ show s2 ++ " at " ++ show pt
        Nothing -> putStrLn $ show s1 ++ " intersects " ++ show s2 ++ " (no single point)"

-- Convert a list of points into segments, each pair forms one segment
pointsToSegments :: [Point] -> Array Int Segment
pointsToSegments pts
  | even (length pts) = listArray (0, length pts `div` 2 - 1) (go pts)
  | otherwise = error "pointsToSegments: odd number of points"
  where
    go (p1 : p2 :rest) = (p1,p2) : go rest
    go [] = []
    go _ = error "pointsToSegments: unexpected"

prettySegments :: Array Int Segment -> String
prettySegments arr = unlines [ show x1 ++ " " ++ show y1 ++ " -> " ++ show x2 ++ " " ++ show y2
  | ((x1,y1),(x2,y2)) <- elems arr ]

intersect :: Int -> IO ()
intersect n
  | n <= 2 = putStrLn "Number should be greater than 2"
  | otherwise = do
      points <- generate (2 * n)
      let segments = pointsToSegments points
      putStrLn "Generated Lines:"
      putStrLn $ prettySegments segments
      putStrLn "Intersection:"
      count <- isIntersect segments
      putStrLn $ "Number of intersections: " ++ show count




