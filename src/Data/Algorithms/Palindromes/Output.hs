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

This module contains functions that apply different ways of formatting the output of the algorithm functions (found in Data.Algorithms.Palindromes.Algorithms).
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

import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (..)
    )
import Data.Algorithms.Palindromes.PreProcessing
    ( filterLetters'
    , textToWordsWithIndices
    )
import Data.List (intercalate)

import qualified Data.Vector as V

{- | Takes a start and an end index in the filtered string and returns the indices
in the unfiltered string
-}
indicesInOutputText :: (Int, Int) -> String -> (Int, Int)
indicesInOutputText (start', end') input
    | start' >= length originalIndices = (length input, length input)
    | end' - start' > 0 = (start, end)
    | otherwise = (start, start)
  where
    originalIndices = filterLetters' input
    start = fst $ originalIndices V.! start'
    end = fst (originalIndices V.! (end' - 1)) + 1

{- | Takes a start and end index in the list of words and returns the start and end indices
of the text of the word palindrome in the original string
-}
indicesInOutputWord :: (Int, Int) -> String -> (Int, Int)
indicesInOutputWord (start', end') input
    | start' >= length wordsWithIndices =
        (maxIndex, maxIndex)
    | end' - start' > 0 = (startIndex, endIndex)
    | otherwise = (startIndex, startIndex)
  where
    wordsWithIndices :: V.Vector ((Int, Int), String)
    wordsWithIndices = textToWordsWithIndices input

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
indicesToText :: (Int, Int) -> String -> String
indicesToText (start, end) input
    | end - start > 0 = take (end - start) $ drop start input
    | otherwise = ""

-- | Returns the length of the longest palindrome as a string
longestLength :: [Int] -> String
longestLength = show . maximum

-- | Converts the longest palindrome to text
longestWord :: [Palindrome] -> String
longestWord input = palText longest
  where
    longest :: Palindrome
    longest = foldr1 (\a b -> if palLength a < palLength b then b else a) input

-- | All maximal palindrome lengths
allLengths :: [Int] -> String
allLengths = show

{- | All maximal palindromes as a list of text
Same as show $ map palText input except this doesn't apply show to the palindrome strings as that will turn \n into \\n
-}
allWords :: [Palindrome] -> String
allWords input = "[" ++ intercalate "," (map (\x -> "\"" ++ palText x ++ "\"") input) ++ "]"

lengthAt :: Int -> [Int] -> String
lengthAt n lengths = show $ lengths !! n

wordAt :: Int -> [Palindrome] -> String
wordAt n pals = palText $ pals !! n