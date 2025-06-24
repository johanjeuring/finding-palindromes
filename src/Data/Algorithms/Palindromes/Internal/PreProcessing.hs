{- |
Module      :  Data.Algorithms.Palindromes.Internal.PreProcessing
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

Functions for converting string input to different variants that can be used in the
algorithms.
-}
module Data.Algorithms.Palindromes.Internal.PreProcessing
    ( filterLetters
    , filterLetters'
    , textToWords
    , textToWordIndices
    , tryParseDNA
    ) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Maybe (fromMaybe)

import Data.Algorithms.Palindromes.DNA
    ( DNA
    , toDNA
    )
import Data.Algorithms.Palindromes.Internal.RangeFunctions (Range)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- | A function that filters the string so that only letters remain.
filterLetters :: U.Vector Char -> U.Vector Char
filterLetters x = U.map toLower $ U.filter isAlphaNum x

{- | A function that filters the string so that only letters remain, but remembers the
original index of each character.
-}
filterLetters' :: U.Vector Char -> U.Vector Int
filterLetters' x = U.map fst $ U.filter (isAlphaNum . snd) (U.indexed (U.map toLower x))

{- | A function that filters the string so that only letters and spaces remain, then
splits the result on every space so that only words remain.
-}
textToWords :: U.Vector Char -> V.Vector String
textToWords x =
    V.fromList $ words $ map toLower $ filter (\a -> isAlphaNum a || isSpace a) $ U.toList x

-- | A function that returns a vector of ranges corresponding to the ranges of every word in the original input text.
textToWordIndices :: U.Vector Char -> V.Vector Range
textToWordIndices input = V.fromList $ map toWordRange $ wordIndices indexedCharacters
  where
    indexedCharacters :: [(Int, Char)]
    indexedCharacters = U.toList $ filterSpaceAndLetters input

    -- Convert a list of indexed characters to the range of the word it represents.
    toWordRange :: [Int] -> Range
    toWordRange [] = error "Empty string"
    toWord word@(firstIndex : _) =
        (firstIndex, last word + 1)

    -- The words function as written in Prelude, but on indexed characters.
    wordIndices :: [(Int, Char)] -> [[Int]]
    wordIndices s
        | null checking = []
        | otherwise = map fst word : wordIndices remaining
      where
        checking = dropWhile (isSpace . snd) s
        (word, remaining) = break (isSpace . snd) checking

    {- Keep only (lowercase) letters and spaces. Returns a vector of tuples with the
    original index of the charactar along with the character. -}
    filterSpaceAndLetters :: U.Vector Char -> U.Vector (Int, Char)
    filterSpaceAndLetters w =
        U.filter
            (\x -> (isAlphaNum . snd) x || (isSpace . snd) x)
            (U.indexed (U.map toLower w))

-- | If trying to parse the string to DNA would fail, throw a more readable error.
tryParseDNA :: U.Vector Char -> U.Vector DNA
tryParseDNA input = fromMaybe (error "Invalid DNA string") (parseDna input)

{- | Filters the string so that only letters remain and parses it to DNA. Returns Nothing
if the string cannot be fully parsed to DNA.
-}
parseDna :: U.Vector Char -> Maybe (U.Vector DNA)
parseDna = toDNA . filterLetters
