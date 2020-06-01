module Pb20
    ( run
    ) where

import Data.Maybe as Maybe
import Data.List as List

-- Pb20
-- Given two singly linked lists that intersect at some point, find the intersecting node. The lists are non-cyclical.
--
-- For example, given A = 3 -> 7 -> 8 -> 10 and B = 99 -> 1 -> 8 -> 10, return the node with value 8.
--
-- In this example, assume nodes with the same value are the exact same node objects.
--
-- Do this in O(M + N) time (where M and N are the lengths of the lists) and constant space.

run :: IO ()
run = let
    a = [3, 7, 8, 10]
    b = [99, 1, 8, 10]
    intersect = intersectList a b
    intersectNode = Maybe.listToMaybe intersect
  in
    do
      putStr "full intersection: "
      print intersect
      putStr "Intersection node: "
      print intersectNode


-- Find the intersection list
intersectList :: [Int] -> [Int] -> [Int]
intersectList a b = let
    zipped = zip (reverse a) (reverse b)
  in
    reverse $ List.unfoldr intersecting zipped


-- Unpack if both elements are the same
intersecting :: [(Int, Int)] -> Maybe (Int, [(Int, Int)])
intersecting ((x, y):lst) | x == y = Just (x, lst)
intersecting _ = Nothing


-- Deprecated way to get the intersection node
intersectNode :: [Int] -> [Int] -> Maybe Int
intersectNode a b = let
    a' = reverse a
    b' = reverse b
    zipped = zip a' b'
    -- Solution if same value === same node (so also same next values)
    intersectionFinder prev (x, y) = if x == y then Just x else prev
  in
    foldl intersectionFinder Nothing zipped
