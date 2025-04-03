module Data.Algorithms.Palindromes.RangeFunctions where

{- |
  Convert a list of palindrome center lengths to a list of (start, end) pairs
-}
lengthsToRanges :: [Int] -> [(Int, Int)]
lengthsToRanges lengths = map indexedLengthToRange indexedLengths
  where
    indexedLengths :: [(Int, Int)]
    indexedLengths = zip [0 :: Int ..] lengths

indexedLengthToRange :: (Int, Int) -> (Int, Int)
indexedLengthToRange (index, len) = (startIndex, endIndex)
  where
    startIndex :: Int
    startIndex = (index `div` 2) - (len `div` 2)
    endIndex :: Int
    endIndex = startIndex + len

rangesToLengths :: [(Int, Int)] -> [Int]
rangesToLengths = map rangeToLength

rangeToLength :: (Int, Int) -> Int
rangeToLength (start, end)
    | end - start < 0 = 0
    | otherwise = end - start