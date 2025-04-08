{- |
Module      :  Data.Algorithms.Palindromes.RangeFunctions
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Describes functions that do range calculations.
-}
module Data.Algorithms.Palindromes.RangeFunctions
    ( lengthsToRanges
    , indexedLengthToRange
    , rangesToLengths
    , rangeToLength
    ) where

-- | Converts a list of palindrome center lengths to a list of (start, end) pairs.
lengthsToRanges :: [Int] -> [(Int, Int)]
lengthsToRanges lengths = map indexedLengthToRange indexedLengths
  where
    indexedLengths :: [(Int, Int)]
    indexedLengths = zip [0 :: Int ..] lengths

{- | Converts a tuple with index and length to a tuple with the starting index and the
  end index.
-}
indexedLengthToRange :: (Int, Int) -> (Int, Int)
indexedLengthToRange (index, len) = (startIndex, endIndex)
  where
    startIndex :: Int
    startIndex = (index `div` 2) - (len `div` 2)
    endIndex :: Int
    endIndex = startIndex + len

-- | Converts a list of (start, end) tuples to a list of palindrome lengths.
rangesToLengths :: [(Int, Int)] -> [Int]
rangesToLengths = map rangeToLength

-- | Converts a (start, end) tuple to a palindrome length.
rangeToLength :: (Int, Int) -> Int
rangeToLength (start, end)
    | end - start < 0 = 0
    | otherwise = end - start