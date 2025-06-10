{-# LANGUAGE BangPatterns #-}

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
    , rangeToText
    , longest
    , allLengths
    , allWords
    , lengthAt
    , wordAt
    ) where

import Data.List (find, foldl', intercalate)

import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (..)
    , getLength
    )
import Data.Algorithms.Palindromes.RangeFunctions (rangeToCenter)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

{- | Takes a start and an end index in the filtered string and returns the indices
in the unfiltered string
-}
indicesInOutputText :: (Int, Int) -> Int -> U.Vector (Int, Char) -> (Int, Int)
indicesInOutputText (start', end') !inputLength originalIndices
    | start' >= U.length originalIndices = (inputLength, inputLength)
    | end' - start' > 0 = (start, end)
    | otherwise = (start, start)
  where
    start = fst $ originalIndices U.! start'
    end = fst (originalIndices U.! (end' - 1)) + 1

{- | Takes a start and end index in the list of words and returns the start and end
indices of the text of the word palindrome in the original string
-}
indicesInOutputWord :: (Int, Int) -> Int -> V.Vector ((Int, Int), String) -> (Int, Int)
indicesInOutputWord (start', end') !inputLength wordsWithIndices
    | start' >= length wordsWithIndices =
        (inputLength, inputLength)
    | end' - start' > 0 = (startIndex, endIndex)
    | otherwise = (startIndex, startIndex)
  where
    firstWord :: ((Int, Int), String)
    firstWord = wordsWithIndices V.! start'
    lastWord :: ((Int, Int), String)
    lastWord = wordsWithIndices V.! (end' - 1)

    startIndex :: Int
    startIndex = fst $ fst firstWord
    endIndex :: Int
    endIndex = snd (fst lastWord)

-- | Takes a start and end index (exclusive) and returns the substring in the text with that range
rangeToText :: (Int, Int) -> U.Vector Char -> String
rangeToText (start, end) input
    | end - start > 0 = U.toList $ U.slice start (end - start) input
    | otherwise = ""

    longest :: [Palindrome] -> Palindrome -> [Palindrome]
    longest [] p = [p]
    longest pals@(p1 : _) p2
        | getLength p1 == getLength p2 = p2 : pals
        | getLength p1 < getLength p2 = [p2]
        | otherwise = pals

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
