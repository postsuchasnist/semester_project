-- module responsible for computing intersections between segments

module Intersect (intersect) where

import Data.List (sortBy, delete)
import qualified Data.Set as Set
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Util
import Generator
import Text.Printf


-- type that defines a segment as a tuple of two endpoints
type Segment = (Point, Point)

data Event = Event
  { ex :: Double
  , ey :: Double
  , isLeft :: Bool
  , segIdx :: Int
  } deriving (Eq, Show)

instance Ord Event where
  compare = comparing (\(Event x y l _) -> (x, y, not l))

--checking if the point is lying on the segment between two other points
onSegment :: Point -> Point -> Point -> Bool
onSegment (px, py) (qx, qy) (rx, ry) =
  qx <= max px rx && qx >= min px rx &&
  qy <= max py ry && qy >= min py ry

orientation :: Point -> Point -> Point -> Int
orientation (px, py) (qx, qy) (rx, ry) =
  let val = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)
  in if abs val < 1e-9 then 0 else if val > 0 then 1 else 2


-- function that checks two segments for intersection and returns true if so
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

-- find the intersection point of two segments (assuming they intersect)
intersectPoint :: Segment -> Segment -> Maybe Point
intersectPoint ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  let denom = (x1 - x2)*(y3 - y4) - (y1 - y2)*(x3 - x4)
  in if abs denom < 1e-9
     then Nothing -- lines are parallel
     else
       let x = ((x1*y2 - y1*x2)*(x3 - x4) - (x1 - x2)*(x3*y4 - y3*x4)) / denom
           y = ((x1*y2 - y1*x2)*(y3 - y4) - (y1 - y2)*(x3*y4 - y3*x4)) / denom
       in Just (x, y)

-- Sweep Line algorithm to find intersections
isIntersect :: [Segment] -> IO Int
isIntersect segments = go sortedEvents Set.empty S.empty
  where
    indexedSegments = zip [0..] segments
    events = concatMap (\(i, ((x1, y1), (x2, y2))) ->
              [Event x1 y1 True i, Event x2 y2 False i]) indexedSegments
    sortedEvents = sortBy (comparing ex) events

    go [] _ reported = do
      putStrLn "Intersecting Lines:"
      mapM_ printIntersect (S.toList reported)
      return $ S.size reported

    --sweep line, where active is the set which is intersected by sweep line, and reported
    -- stores pairs for which intersection has been detected
    go (e : es) active reported =
      let i = segIdx e
          seg = segments !! i
          activeList = Set.toAscList active
          prev = safeGetPrev e activeList
          next = safeGetNext e activeList
          tryInsert a b s =
            if doIntersect (segments !! a) (segments !! b)
              then S.insert (min a b, max a b) s
              else s
      in if isLeft e
         then
           let active' = Set.insert e active
               reported' = foldr (\x acc -> tryInsert (segIdx x) i acc) reported (filter (/= e) activeList)
           in go es active' reported'
         else
           let active' = Set.delete (Event (fst $ fst seg) (snd $ fst seg) True i) active
               reported' = case (prev, next) of
                             (Just p, Just n) -> tryInsert (segIdx p) (segIdx n) reported
                             _ -> reported
           in go es active' reported'

    safeGetPrev _ [] = Nothing
    safeGetPrev _ [_] = Nothing
    safeGetPrev e (x : y : xs)
      | y == e = Just x
      | otherwise = safeGetPrev e (y : xs)

    safeGetNext _ [] = Nothing
    safeGetNext _ [_] = Nothing
    safeGetNext e (x : y : xs)
      | x == e = Just y
      | otherwise = safeGetNext e (y : xs)

    printIntersect (i, j) = do
      let s1 = segments !! i
          s2 = segments !! j
      case intersectPoint s1 s2 of
        Just pt -> putStrLn $ show s1 ++ " intersects " ++ show s2 ++ " at " ++ show pt
        Nothing -> putStrLn $ show s1 ++ " intersects " ++ show s2 ++ " (no single point)"

--an auxilary function to convert points to segments, the segments form a polyline and
-- we are interested in the intersections of inner sides, i.e. we dont count endpoints intersections
pointsToSegments :: [Point] -> [Segment]
pointsToSegments pts = zip pts (tail pts)

--an auxilary function to display segments 
prettySegments :: [Segment] -> String
prettySegments = unlines . map (\((x1, y1), (x2, y2)) ->
  show x1 ++ " " ++ show y1 ++ " -> " ++ show x2 ++ " " ++ show y2)

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


