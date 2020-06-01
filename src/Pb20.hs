module Pb20
    ( run
    ) where


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
    res = intersectNode a b
  in
    putStrLn $ show res


intersectNode :: [Int] -> [Int] -> Maybe Int
intersectNode a b = let
    a' = reverse a
    b' = reverse b
    zipped = zip a' b'
    -- Solution if same value === same node (so also same next values)
    intersectionFinder = \prev (x, y) -> if x == y then Just x else prev
  in
    foldl intersectionFinder Nothing zipped

