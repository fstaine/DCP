module Pb28
    ( solve ) where


solve :: IO()
solve = let
    width = 17
    input = ["the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"]
    response = justify width input
  in putStrLn . show $ response


justify :: [String] -> [String]
justify width words = let 
        wordsByLines = foldl (groupByMaxWordsCount width) ([], []) words 
        justifiedLines = map (justifyLine width) wordsByLines
    in justifiedLines


groupByMaxWordsCount :: Int -> ([[String]], [String]) -> String -> ([[String]], [String])
groupByMaxWordsCount width (done, tmp) word = let
        nextTmpLengthWithSpace = map (1 + length) (word:tmp)
        totalLength = sum nextTmpLengthWithSpace - 1 
    in if totalLength > width
        then (reverse tmp: done, [word]) 
        else (done, word: tmp)


justifyLine :: Int -> [String] -> String
justifyLine width words = let
        wordsCount = length words
        wordsSpaceCount = wordsCount - 1
        margin = width - sum $ map length words
        fixSpacesCount = margin `div` (length wordsSpaceCount)
        additionalSpaceIndex = margin `mod` (length wordsSpaceCount)
        noSpaceIndex = length words - 1
    in 
        concat $ map () $ zip words [1..]
