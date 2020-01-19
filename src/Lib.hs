module Lib
    ( pb1
    , pb2
    , pb3
    ) where

import Control.Monad (filterM)
import Data.List as List
import Data.String as String
import Data.Tuple as Tuple



{-
------- Utils --------
-}



splitOn :: Char -> String -> [String]
splitOn = splitOn' ""

splitOn' :: String -> Char -> String -> [String]
splitOn' acc c [] = [reverse acc]
splitOn' acc c (x:xs) = if c == x then reverse acc : splitOn' [] c xs else splitOn' (x:acc) c xs


mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, c) = (f a, c)


{-
------- Problems --------
-}



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


-- pb3
-- Given the root to a binary tree, implement serialize(root), which serializes the tree into a string, and deserialize(s), which deserializes the string back into the tree.

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)


pb3 :: IO()
pb3 = let
    tree = Branch "root"
        (Branch "left"
            Empty
            (Branch "left.right"
                Empty
                Empty) )
        (Branch "right" Empty Empty)
    s = serialize tree
    d = deserialize s
    s' = serialize tree
    in
      putStrLn $ show s


serialize :: Tree String -> String
serialize Empty = ""
serialize (Branch content left right) = content ++ ";" ++ (serialize left) ++ ";" ++ (serialize right)

deserialize :: String -> Tree String
deserialize s = Tuple.fst $ deserialize' $ Split.splitOn "," s

deserialize' :: [String] -> (Tree String, [String])
deserialize' [] = (Empty, [])
deserialize' (x:xs) = let
        (left, rest1) = deserialize' xs
        (right, rest2) = deserialize' rest1
    in
        (Branch x left right, rest2)


--pb4
--This problem was asked by Stripe.
--Given an array of integers, find the first missing positive integer in linear time and constant space. In other words, find the lowest positive integer that does not exist in the array. The array can contain duplicates and negative numbers as well.
--For example, the input [3, 4, -1, 1] should give 2. The input [1, 2, 0] should give 3.
--You can modify the input array in-place.

pb4 :: IO()
pb4 = let
    input = [3, 4, -1, 1]
    response = fakeSolvePb4 input
  in
    putStrLn . show $ response

fakeSolvePb4 :: [Int] -> Int
fakeSolvePb4 l = head $ filter (\x -> not $ elem x l) [1..]


--pb7
--This problem was asked by Facebook.
--Given the mapping a = 1, b = 2, ... z = 26, and an encoded message, count the number of ways it can be decoded.
--For example, the message '111' would give 3, since it could be decoded as 'aaa', 'ka', and 'ak'.
--You can assume that the messages are decodable. For example, '001' is not allowed.

pb7 :: IO()
pb7 = let
    msg = "111"
    response = solvePb7 msg
  in
    putStrLn . show $ response

solvePb7 :: String -> Int
solvePb7 = solvePb7'

solvePb7' :: String -> Int
solvePb7' [] = 1
solvePb7' ('0' : _) = 0
solvePb7' (_ : []) = 1
solvePb7' ('1' : x : xs) =  (solvePb7' xs) + (solvePb7' $ x : xs)
solvePb7' ('2' : x : xs) = (solvePb7' xs) + if elem x "123456" then (solvePb7' $ x : xs) else 0
solvePb7' (_ : xs) = solvePb7' xs

--pb8
--This problem was asked by Google.
--A unival tree (which stands for "universal value") is a tree where all nodes under it have the same value.
--Given the root to a binary tree, count the number of unival subtrees.
--For example, the following tree has 5 unival subtrees:

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

--type Leaf a = Branch a Empty Empty

pb8 :: IO()
pb8 = let
    tree = Branch 0
        (Branch 1 Empty Empty)
        (Branch 0
          (Branch 1
            (Branch 1 Empty Empty)
            (Branch 1 Empty Empty)
          )
          (Branch 0 Empty Empty)
        )
    result = solvePb8 tree
  in
    putStrLn $ show result


solvePb8 :: (Eq a) => Tree a -> Int
solvePb8 = solvePb8'

solvePb8' :: (Eq a) => Tree a -> Int
solvePb8' Empty = 0
solvePb8' (Branch _ Empty Empty) = 1
solvePb8' (Branch x l r) = case (l, r) of
  ((Branch lx ll lr), (Branch rx rl rr)) -> solvePb8' l + solvePb8' r + if x == lx && x == rx then 1 else 0
  otherwise -> solvePb8' l + solvePb8' r


--pb12
--This problem was asked by Amazon.
--There exists a staircase with N steps, and you can climb up either 1 or 2 steps at a time. Given N, write a function that returns the number of unique ways you can climb the staircase. The order of the steps matters.
--For example, if N is 4, then there are 5 unique ways:
--    1, 1, 1, 1
--    2, 1, 1
--    1, 2, 1
--    1, 1, 2
--    2, 2
--What if, instead of being able to climb 1 or 2 steps at a time, you could climb any number from a set of positive integers X? For example, if X = {1, 3, 5}, you could climb 1, 3, or 5 steps at a time.

--pb12 :: IO ()
--pb12 = let
--    size = 4
--    response = solvePb12 size
--  in
--    putStrLn show $ response
--
--
--solvePb12 :: Int -> Int
--solvePb12


--pb17
--Suppose we represent our file system by a string in the following manner:
--The string "dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext" represents:
--dir
--    subdir1
--    subdir2
--        file.ext
--The directory dir contains an empty sub-directory subdir1 and a sub-directory subdir2 containing a file file.ext.
--
--The string "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext" represents:
--dir
--    subdir1
--        file1.ext
--        subsubdir1
--    subdir2
--        subsubdir2
--            file2.ext
--The directory dir contains two sub-directories subdir1 and subdir2. subdir1 contains a file file1.ext and an empty second-level sub-directory subsubdir1. subdir2 contains a second-level sub-directory subsubdir2 containing a file file2.ext.
--
--We are interested in finding the longest (number of characters) absolute path to a file within our file system. For example, in the second example above, the longest absolute path is "dir/subdir2/subsubdir2/file2.ext", and its length is 32 (not including the double quotes).
--Given a string representing the file system in the above format, return the length of the longest absolute path to a file in the abstracted file system. If there is no file in the system, return 0.
--Note:
--The name of a file contains at least a period and an extension.
--The name of a directory or sub-directory will not contain a period.

--pb17 :: IO()
--pb17 = let
--    input = "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext"
--    response = solvePb17 input
--  in
--    putStrLn show $ response
--
--solvePb17 :: String -> Int

isFile :: String -> Bool
isFile = elem '.'

splitPaths :: String -> [String]
splitPaths = splitOn '\n'

splitDepthName :: String -> (Int, String)
splitDepthName = splitDepthName'

splitDepthName' :: String -> (Int, String)
splitDepthName' ('\t':xs) = mapFirst (+ 1) . splitDepthName' $ xs
splitDepthName' l = (0, l)
