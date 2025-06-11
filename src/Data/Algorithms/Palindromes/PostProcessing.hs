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
    ( filterPunctuation
    ) where

import Data.Char (isLetter)

import Data.Algorithms.Palindromes.PreProcessing (filterLetters')
import Data.Algorithms.Palindromes.RangeFunctions (Range)

import qualified Data.Vector as V

-- | This function changes the a list of ranges for punctuation palindromes by shrinking to punctuation.
filterPunctuation :: String -> [Range] -> [Range]
filterPunctuation input = map shrinkRange
  where
    {- Shrinks a range on both sides until the resulting range is surrounded by
    punctuation in the original input. -}
    shrinkRange :: Range -> Range
    shrinkRange (startIndex, endIndex)
        | startIndex == endIndex = (startIndex, endIndex)
        | startIndex > endIndex = (endIndex, endIndex)
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
