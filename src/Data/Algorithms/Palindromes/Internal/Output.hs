{-# LANGUAGE BangPatterns #-}

{- |
Module      :  Data.Algorithms.Palindromes.Internal.Output
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
It also contains the logic to convert a palindrome range to the text the range represents.
-}
module Data.Algorithms.Palindromes.Internal.Output
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

import Data.Algorithms.Palindromes.Internal.RangeFunctions (Range)
import Data.Algorithms.Palindromes.Palindrome
    ( Palindrome (..)
    , getLength
    )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

{- | Takes a start and an end index in the filtered string and returns the indices
in the unfiltered string.
-}
indicesInOutputText
    :: Range
    -- ^ The range of the palindrome range in the filtered vector.
    -> Int
    -- ^ The length of the original input.
    -> U.Vector Int
    -- ^ The indices of every element in the filtered vector in the original string.
    -> Range
    -- ^ The range of the paldinrome in the original string.
indicesInOutputText (start', end') !inputLength originalIndices
    | start' >= U.length originalIndices = (inputLength, inputLength)
    | end' - start' > 0 = (start, end)
    | otherwise = (start, start)
  where
    start = originalIndices U.! start'
    end = (originalIndices U.! (end' - 1)) + 1

{- | Takes a range in the vector containing words and returns the range of the text
of the word palindrome in the original string.
-}
indicesInOutputWord
    :: Range
    -- ^ The range of the word palindrome in the vector of words.
    -> Int
    -- ^ The length of the original text.
    -> V.Vector Range
    -- ^ A vector containing the indices in the original text of every word in that text.
    -> Range
indicesInOutputWord (start', end') !inputLength wordsWithIndices
    | start' >= length wordsWithIndices =
        (inputLength, inputLength)
    | end' - start' > 0 = (startIndex, endIndex)
    | otherwise = (startIndex, startIndex)
  where
    firstWord :: Range
    firstWord = wordsWithIndices V.! start'
    lastWord :: Range
    lastWord = wordsWithIndices V.! (end' - 1)

    startIndex :: Int
    startIndex = fst firstWord
    endIndex :: Int
    endIndex = snd lastWord

-- | Takes a start and end index (exclusive) and returns the substring in the text with that range
rangeToText :: Range -> U.Vector Char -> String
rangeToText (start, end) input
    | end - start > 0 = U.toList $ U.slice start (end - start) input
    | otherwise = ""

{- | foldl helper function that makes sure the resulting list only contains the longest
palindromes out of the input list.
-}
longest :: [Palindrome] -> Palindrome -> [Palindrome]
longest [] p = [p]
longest pals@(p1 : _) p2
    | getLength p1 == getLength p2 = p2 : pals
    | getLength p1 < getLength p2 = [p2]
    | otherwise = pals

-- | All maximal palindrome lengths.
showLengths :: [Palindrome] -> String
showLengths pals = show $ map getLength pals

-- | Show all palindromes as a list of strings, separated by a newline.
showTexts :: [Palindrome] -> String
showTexts pals = intercalate "\n" (map (\x -> "\"" ++ palText x ++ "\"") pals)

-- | Show all ranges of the palindromes.
showRanges :: [Palindrome] -> String
showRanges pals = show $ map palRangeInText pals

-- | Show all details of the palindromes: text, range and length.
showAll :: [Palindrome] -> String
showAll pals = intercalate "\n" (map palToDetailString pals)
  where
    palToDetailString pal = "\"" ++ palText pal ++ "\" " ++ show (palRange pal) ++ " " ++ show (getLength pal)
