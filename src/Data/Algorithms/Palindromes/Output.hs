{-# LANGUAGE BangPatterns #-}

{- |
Module      :  Data.Algorithms.Palindromes.Output
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module contains functions that apply different ways of formatting the output of the
algorithm functions (found in Data.Algorithms.Palindromes.Algorithms).
-}
module Data.Algorithms.Palindromes.Output
    ( indicesInOutputText
    , indicesInOutputWord
    , rangeToText
    , longest
    , showLengths
    , showTexts
    , showRanges
    , showAll
    ) where

import Data.List (intercalate)

import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (..)
    , getLength
    )
import Data.Algorithms.Palindromes.RangeFunctions (Range)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

{- | Takes a start and an end index in the filtered string and returns the indices
in the unfiltered string
-}
indicesInOutputText :: Range -> Int -> U.Vector Int -> Range
indicesInOutputText (start', end') !inputLength originalIndices
    | start' >= U.length originalIndices = (inputLength, inputLength)
    | end' - start' > 0 = (start, end)
    | otherwise = (start, start)
  where
    start = originalIndices U.! start'
    end = (originalIndices U.! (end' - 1)) + 1

{- | Takes a range in the vector containing words and returns the range of the text
of the word palindrome in the original string
-}
indicesInOutputWord :: Range -> Int -> V.Vector (Range, String) -> Range
indicesInOutputWord (start', end') !inputLength wordsWithIndices
    | start' >= length wordsWithIndices =
        (inputLength, inputLength)
    | end' - start' > 0 = (startIndex, endIndex)
    | otherwise = (startIndex, startIndex)
  where
    firstWord :: (Range, String)
    firstWord = wordsWithIndices V.! start'
    lastWord :: (Range, String)
    lastWord = wordsWithIndices V.! (end' - 1)

    startIndex :: Int
    startIndex = fst $ fst firstWord
    endIndex :: Int
    endIndex = snd (fst lastWord)

-- | Takes a start and end index (exclusive) and returns the substring in the text with that range
rangeToText :: Range -> U.Vector Char -> String
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
showLengths :: [Palindrome] -> String
showLengths pals = show $ map getLength pals

-- | All maximal palindromes as a list of strings, separated by a newline.
showTexts :: [Palindrome] -> String
showTexts pals = intercalate "\n" (map (\x -> "\"" ++ palText x ++ "\"") pals)

showRanges :: [Palindrome] -> String
showRanges pals = show $ map palRangeInText pals

showAll :: [Palindrome] -> String
showAll pals = intercalate "\n" (map palToDetailString pals)
  where
    palToDetailString pal = "\"" ++ palText pal ++ "\" " ++ show (palRange pal) ++ " " ++ show (getLength pal)
