{- |
Module      :  Data.Algorithms.Palindromes.PostProcessing
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Describes postprocessing functions. The Palindromes package uses these to apply a length
modifier if one is set, and for the punctuation type to shorten palindromes to
punctuation.
-}
module Data.Algorithms.Palindromes.PostProcessing
    ( filterMin
    , filterMax
    , filterExact
    , filterPunctuation
    ) where

import Data.Char (isLetter)

import Data.Algorithms.Palindromes.PreProcessing (filterLetters')
import Data.Algorithms.Palindromes.RangeFunctions
    ( lengthsToRanges
    , rangeToLength
    )

import qualified Data.Vector as V

-- Length modifier filtering
filterMin :: Int -> [Int] -> [Int]
filterMin 0 = id
filterMin min_ = map (\x -> if x < min_ then 0 else x)
filterMax :: Maybe Int -> [Int] -> [Int]
filterMax Nothing = id
filterMax (Just max_) = map (\x -> if x > max_ then 0 else x)
filterExact :: Int -> [Int] -> [Int]
filterExact n = map (\x -> if x == n then x else 0)

{- | This function changes a list of text palindrome lengths to a list of punctuation
palindrome lengths by making the lengths shorter where needed.
-}
filterPunctuation :: String -> [Int] -> [Int]
filterPunctuation input lengths = map (rangeToLength . shrinkRange) $ lengthsToRanges lengths
  where
    {- Shrinks a range on both sides until the resulting range is surrounded by
    punctuation in the original input. -}
    shrinkRange :: (Int, Int) -> (Int, Int)
    shrinkRange (startIndex, endIndex)
        | startIndex == endIndex = (startIndex, endIndex)
        | startIndex > endIndex = (startIndex, endIndex)
        | punctuationAt (originalStart - 1) && punctuationAt (originalEnd + 1) =
            (startIndex, endIndex)
        | otherwise = shrinkRange (startIndex + 1, endIndex - 1)
      where
        originalStart :: Int
        originalStart = fst $ filterLetters' input V.! startIndex
        originalEnd :: Int
        originalEnd = fst $ filterLetters' input V.! (endIndex - 1)
    punctuationAt :: Int -> Bool
    punctuationAt i
        | i < length input && i >= 0 = (not . isLetter) $ inputAsVector V.! i
        | otherwise = True
    inputAsVector = V.fromList input
