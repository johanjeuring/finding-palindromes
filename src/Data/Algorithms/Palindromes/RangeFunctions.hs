{- |
Module      :  Data.Algorithms.Palindromes.RangeFunctions
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

Describes functions that do range calculations.
-}
module Data.Algorithms.Palindromes.RangeFunctions
    ( lengthsToRanges
    , indexedLengthToRange
    , rangeToLength
    , rangeToPalindromeCenter
    , Range
    ) where

-- | A range with format: ([start index (inclusive)], [end index (exclusive)]).
type Range = (Int, Int)

-- | Converts a list of palindrome center lengths to a list of (start, end) pairs.
lengthsToRanges :: [Int] -> [Range]
lengthsToRanges lengths = map indexedLengthToRange indexedLengths
  where
    indexedLengths :: [(Int, Int)]
    indexedLengths = zip [0 :: Int ..] lengths

{- | Converts a tuple with center index and length to a tuple with the starting character
  index and the end character index.
-}
indexedLengthToRange :: (Int, Int) -> Range
indexedLengthToRange (index, len) = (startIndex, endIndex)
  where
    startIndex :: Int
    startIndex = (index `div` 2) - (len `div` 2)
    endIndex :: Int
    endIndex = startIndex + len

{- Converts a (palindrome's) range to a palindrome length. We take max to cover an edge
case where the range is negative. The palindrome should then be empty. -}
rangeToLength :: Range -> Int
rangeToLength (start, end) = max 0 (end - start)

{- A center, in the context of palindromes, is the position in the string from where
the two arms of the palindrome extend. This can be on a character or inbetween two
characters. Therefore this scales twice as fast, hence we can add both sides of the
range in the string. -}
rangeToPalindromeCenter :: Range -> Int
rangeToPalindromeCenter = uncurry (+)
