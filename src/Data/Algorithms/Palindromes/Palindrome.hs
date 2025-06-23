{- |
Module      :  Data.Algorithms.Palindromes.Palindrome
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

Describes the palindrome datatype.
-}
module Data.Algorithms.Palindromes.Palindrome (Palindrome (..), getLength) where

import Data.Algorithms.Palindromes.Internal.RangeFunctions (Range, rangeToLength)

-- | Data type to represent a single found palindrome
data Palindrome
    = Palindrome
    { palRange :: Range
    -- ^ The range of the palindrome in the pre-pocessed input vector.
    , palText :: String
    {- ^ The text representing the found palindrome. Note that this must be a string,
    not some abstract datatype. This string must be a subarray of the original
    (not pre-processed) input string, meaning that e.g. present punctuation is in this
    string.
    -}
    , palRangeInText :: Range
    {- ^ The start (inclusive) and end (exclusive) index of the palindrome in the original
    string.
    -}
    }
    deriving (Show, Eq)

{- An example text Palindrome from plain input string "bab..ac" is
(Palindrome 5 3 "ab..a" (1,6)). The center is on the 'b' and has center index 5. The
pre-processed text palindrome is "aba", so the length is 3, and after adding back
punctuation, the start character index is 1 (the first 'a') and the end character index
is 6 (the 'c' after the second 'a'). The string representing this text palindrome is
"ab..a". -}

-- | Returns the length of the palindrome in the input vector.
getLength :: Palindrome -> Int
getLength = rangeToLength . palRange
