module Lib
    ( pb1
    , pb2
    , pb3
    ) where

import Control.Monad (filterM)
import Data.List as List

powerset = filterM (const [True, False])

-- Pb1
-- Given a list of numbers and a number k, return whether any two numbers from the list add up to k.
-- Bonus: Can you do this in one pass?


pb1 :: IO ()
pb1 = let 
        l = [10, 15, 3, 8]
        k = 17
        res = solvePb1 l k
    in
        putStrLn $ show res


solvePb1 :: [Int] -> Int -> Bool
solvePb1 [] k = False
solvePb1 (x:xs) k = (List.elem (k-x) xs) || solvePb1 xs k


-- Pb2
-- Given an array of integers, return a new array such that each element at index i of the new array is the product of all the numbers in the original array except the one at i.
-- Follow-up: what if you can't use division?

pb2 :: IO ()
pb2 = let 
        l = [1, 2, 3, 4, 5]
        res = solvePb2 l
    in
        putStrLn $ show res


solvePb2 :: [Int] -> [Int]
solvePb2 l = let 
        prod = product l
    in
        map (div prod) l


-- Given the root to a binary tree, implement serialize(root), which serializes the tree into a string, and deserialize(s), which deserializes the string back into the tree.

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)


pb3 :: IO()
pb3 = let 
    tree = Branch "root" 
        (Branch "left" 
            (Branch "left.left" 
                Empty 
                Empty) 
            Empty) 
        (Branch "right" Empty Empty)
    s = serialize tree
    in
      putStrLn s


serialize :: Tree String -> String
serialize t = case t of 
    Empty -> ""
    Branch content left right -> 
        "(" ++ show content ++ "," ++ (serialize left) ++ "," ++ (serialize right) ++ ")"

deserialize :: String -> Tree String
deserialize = deserialize'

deserialize' :: String -> Tree String
deserialize' "" = Empty
deserialize' s = 
