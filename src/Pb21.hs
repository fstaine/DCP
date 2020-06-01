module Pb21
    ( run
    ) where


import Data.Maybe as Maybe
import Data.List as List

-- Pb21
--This problem was asked by Snapchat.
--
--Given an array of time intervals (start, end) for classroom lectures (possibly overlapping), find the minimum number of rooms required.
--
--For example, given [(30, 75), (0, 50), (60, 150)], you should return 2.

{-
  Real solution is a Graph Coloring problem

  This answer is just a non-optimal approximation,
  as the final number of classes needed will depend of the order of the scheduled classes
-}

run :: IO ()
run = let
    classroomsTimes = [(30, 75), (0, 50), (60, 150), (10, 20)]
    res = assignToRooms classroomsTimes
    count = minClassroomCount classroomsTimes
  in
    do
      print res
      print count


type Schedule = (Int, Int)
type Classroom = [Schedule]


minClassroomCount :: [Schedule] -> Int
minClassroomCount = List.length . assignToRooms


assignToRooms :: [Schedule] -> [Classroom]
assignToRooms = foldl addToFirstAvailable []


--assignToRooms' :: ([Schedule], [Classroom]) -> Maybe (Classroom, ([Schedule], [Classroom]))
--assignToRooms' ([], c) ->

-- Add a Schedule to the first room which is available at that time
-- Create a new room if none is available
addToFirstAvailable :: [Classroom] -> Schedule -> [Classroom]
addToFirstAvailable rooms schedule = unfoldr addToFirstAvailable' (Just schedule, rooms)


canAddToClassroom :: Schedule -> Classroom -> Bool
canAddToClassroom s = not . any (overlap s)


overlap :: Schedule -> Schedule -> Bool
overlap (x1, x2) (y1, y2) = x1 <= y2 && y1 <= x2


addToFirstAvailable' :: (Maybe Schedule, [Classroom]) -> Maybe (Classroom, (Maybe Schedule, [Classroom]))
-- Completed
addToFirstAvailable' (Nothing, []) = Nothing
-- Create a new room as all other are not available
addToFirstAvailable' (Just s, []) = Just ([s], (Nothing, []))
-- Schedule already planned, just add the other rooms like they are
addToFirstAvailable' (Nothing, x : xs) = Just (x, (Nothing, xs))
-- Possible to add to the current classroom, then add it
addToFirstAvailable' (Just s, x : xs) | canAddToClassroom s x = Just (s : x, (Nothing, xs))
-- Let's try to add to another classroom...
addToFirstAvailable' (Just s, x : xs) = Just (x, (Just s, xs))
