-----------------------------------------------------------------------------
--
-- Module      :  Data.Algorithms.Palindromes.PalindromesUtils
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Flags a user can specify
-----------------------------------------------------------------------------
-- Palindromic variants (choose 1 out of 6; mutually exclusive):
-- Algorithm complexity (choose 1 out of 2; mutually exclusive):
-- Output format (choose 1 out of 4; mutually exclusive):
-- Modifiers (choose 0 to 5; where the length restrictions need to fit together)
-- input via AtLeast and AtMost. Adapt?
-- Input format
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Algorithms.Palindromes.PalindromesUtils
    ( (=:=)
    , toDNA
    , Couplable
    , DNA (..)
    , Palindrome (..)
    , rangesToLengths
    , lengthsToRanges
    , indexedLengthToRange
    , rangeToLength
    , dnaToChar
    , couplableWithItselfAtIndex
    ) where

import Data.Maybe (fromJust, isNothing)

import qualified Data.Vector as V

-- | Data type to represent a single found palindrome
data Palindrome
    = Palindrome
    { palCenterIndex :: Int
    {- ^ The index of the center of this found palindrome from the pre-processed input
    vector.
    -}
    , palLength :: Int
    -- ^ The length of the found palindrome in the pre-processed input vector.
    , palText :: String
    {- ^ The text representing the found palindrome. Note that this must be a string,
    not some abstract datatype. This string must be a subarray of the original
    (not pre-processed) input string, meaning that e.g. present punctuation is in this
    string.
    -}
    , palRange :: (Int, Int)
    -- ^ The start (inclusive) and end (exclusive) index of the palindrome in the original string
    }
    deriving (Show, Eq)

{- An example text Palindrome from plain input string "bab..ac" is
(Palindrome 5 3 1 6 "ab..a"). The center is on the 'b' and has center index 5. The
pre-processed text palindrome is "aba", so the length is 3, and after adding back
punctuation, the start character index is 1 (the first 'a') and the end character index
is 6 (the 'c' after the second 'a'). The string representing this text palindrome is
"ab..a". -}

{- |
  Convert a list of palindrome center lengths to a list of (start, end) pairs
-}
lengthsToRanges :: [Int] -> [(Int, Int)]
lengthsToRanges lengths = map indexedLengthToRange indexedLengths
  where
    indexedLengths :: [(Int, Int)]
    indexedLengths = zip [0 :: Int ..] lengths

indexedLengthToRange :: (Int, Int) -> (Int, Int)
indexedLengthToRange (index, len) = (startIndex, endIndex)
  where
    startIndex :: Int
    startIndex = (index `div` 2) - (len `div` 2)
    endIndex :: Int
    endIndex = startIndex + len

rangesToLengths :: [(Int, Int)] -> [Int]
rangesToLengths = map rangeToLength

rangeToLength :: (Int, Int) -> Int
rangeToLength (start, end)
    | end - start < 0 = 0
    | otherwise = end - start

{-
-----------------------------
  Begin couplable definition
-----------------------------
-}

{- |
  Shows that some element belongs to another element.
  For example, A belongs to T in DNA, and 'z' belongs to 'z' in normal text.
-}
class Couplable a where
    (=:=) :: a -> a -> Bool

-- | Define Couplable instance for any a of class Eq. Just use the equality relation.
instance (Eq a) => Couplable a where
    (=:=) = (==)

{-
-----------------------------
  End couplable definition
-----------------------------
-}

{-
--------------------------------------
  Begin DNA definition and functions
--------------------------------------
-}

-- | Datatype for the different DNA, note that (=)/Eq is not suitable for checking if DNA has palindromes, instead couplable should be used
data DNA = A | T | C | G | N deriving (Show, Eq)

-- | Declare instance Couplable for DNA. A and T form a couple, C and G form a couple.
instance {-# OVERLAPPING #-} Couplable DNA where
    A =:= T = True
    T =:= A = True
    G =:= C = True
    C =:= G = True
    _ =:= _ = False

toDNA :: (Functor f, Foldable f) => f Char -> Maybe (f DNA)
toDNA x = if hasNothing then Nothing else Just $ fmap (fromJust . charToDNA) x
  where
    hasNothing = any (isNothing . charToDNA) x

charToDNA :: Char -> Maybe DNA
charToDNA 'A' = Just A
charToDNA 'T' = Just T
charToDNA 'C' = Just C
charToDNA 'G' = Just G
charToDNA 'N' = Just N
charToDNA 'a' = Just A
charToDNA 't' = Just T
charToDNA 'c' = Just C
charToDNA 'g' = Just G
charToDNA 'n' = Just N
charToDNA _ = Nothing

dnaToChar :: DNA -> Char
dnaToChar A = 'A'
dnaToChar T = 'T'
dnaToChar G = 'G'
dnaToChar C = 'C'
dnaToChar N = 'N'

{-
--------------------------------------
  End DNA definition and functions
--------------------------------------
-}

{-
---------------------------------------
  Begin functions for Couplable types
---------------------------------------
-}

{- | Safe function which returns whether an element at an index in the input vector is
  couplable with itself.
-}
couplableWithItselfAtIndex :: (Couplable a) => V.Vector a -> Int -> Bool
couplableWithItselfAtIndex input index
    | index < 0 || index >= V.length input = False
    | otherwise = element =:= element
  where
    element = input V.! index

{-
---------------------------------------
  End functions for Couplable types
---------------------------------------
-}
