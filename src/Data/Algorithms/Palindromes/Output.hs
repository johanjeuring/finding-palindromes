{- |
Module      :  Data.Algorithms.Palindromes.Output
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

This module contains functions that apply different ways of formatting the output of the
algorithm functions (found in Data.Algorithms.Palindromes.Algorithms).
-}
module Data.Algorithms.Palindromes.Output
    ( indicesInOutputText
    , indicesInOutputWord
    , indicesToText
    , longestLength
    , longestWord
    , allLengths
    , allWords
    , lengthAt
    , wordAt
    ) where

import Data.List (find, intercalate)

import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (..)
    )
import Data.Algorithms.Palindromes.RangeFunctions (rangeToCenter, rangeToLength)

import qualified Data.Vector as V

{- | Takes a start and an end index in the filtered string and returns the indices
in the unfiltered string
-}
indicesInOutputText :: (Int, Int) -> String -> V.Vector (Int, Char) -> (Int, Int)
indicesInOutputText (start', end') input originalIndices
    | start' >= length originalIndices = (length input, length input)
    | end' - start' > 0 = (start, end)
    | otherwise = (start, start)
  where
    start = fst $ originalIndices V.! start'
    end = fst (originalIndices V.! (end' - 1)) + 1

{- | Takes a start and end index in the list of words and returns the start and end
indices of the text of the word palindrome in the original string
-}
indicesInOutputWord :: (Int, Int) -> String -> V.Vector ((Int, Int), String) -> (Int, Int)
indicesInOutputWord (start', end') input wordsWithIndices
    | start' >= length wordsWithIndices =
        (maxIndex, maxIndex)
    | end' - start' > 0 = (startIndex, endIndex)
    | otherwise = (startIndex, startIndex)
  where
    maxIndex :: Int
    maxIndex = length input

    firstWord :: ((Int, Int), String)
    firstWord = wordsWithIndices V.! start'
    lastWord :: ((Int, Int), String)
    lastWord = wordsWithIndices V.! (end' - 1)

    startIndex :: Int
    startIndex = fst $ fst firstWord
    endIndex :: Int
    endIndex = snd (fst lastWord)

-- | Takes a start and end index (exclusive) and returns the substring with those indices
indicesToText :: (Int, Int) -> V.Vector Char -> String
indicesToText (start, end) input
    | end - start > 0 = V.toList $ V.slice start (end - start) input
    | otherwise = ""

-- | Returns the length of the longest palindrome as a string
longestLength :: [Int] -> String
longestLength = show . maximum

-- | Converts the longest palindrome to text
longestWord :: [Palindrome] -> String
longestWord [] = ""
longestWord input = palText $ foldr1 longest input
  where
    longest :: Palindrome -> Palindrome -> Palindrome
    longest p1 p2
        | rangeToLength (palRange p1) < rangeToLength (palRange p2) = p2
        | otherwise = p1

-- | All maximal palindrome lengths
allLengths :: [Int] -> String
allLengths = show

{- | All maximal palindromes as a list of strings. Same as show $ map palText input except
this doesn't apply show to the palindrome strings as that will turn \n into \\n.
-}
allWords :: [Palindrome] -> String
allWords input = "[" ++ intercalate "," (map (\x -> "\"" ++ palText x ++ "\"") input) ++ "]"

-- | Get the length of the maximal palindrome at the specified center index as a string.
lengthAt :: Int -> [Int] -> String
lengthAt n lengths = show $ lengths !! n

-- | Get the maximal palindrome string at the specified center index.
wordAt :: Int -> [Palindrome] -> String
wordAt n pals = maybe "" palText pal
  where
    pal :: Maybe Palindrome
    pal = find ((== n) . rangeToCenter . palRange) pals
