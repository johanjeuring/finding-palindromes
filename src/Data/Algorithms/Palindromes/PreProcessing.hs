{- |
Module      :  Data.Algorithms.Palindromes.PreProcessing
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

Functions for converting string input to different variants that can be used in the
algorithms.
-}
module Data.Algorithms.Palindromes.PreProcessing
    ( filterLetters
    , filterLetters'
    , textToDNA
    , textToWords
    , textToWordsWithIndices
    , tryParseDNA
    ) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Maybe (fromMaybe)

import Data.Algorithms.Palindromes.DNA
    ( DNA
    , toDNA
    )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- Make sure all functions are of the type
-- (PalEq b) => String -> [b]

-- | A function that filters the string so that only letters remain
filterLetters :: String -> U.Vector Char
filterLetters x = U.map toLower $ U.filter isAlphaNum (U.fromList x)

{- | A function that filters the string so that only letters remain, but remembers the
original index of each character.
-}
filterLetters' :: String -> U.Vector (Int, Char)
filterLetters' x = U.filter (isAlphaNum . snd) (U.indexed (U.map toLower (U.fromList x)))

-- | A function that parses ATGCN to the DNA datatype
textToDNA :: String -> Maybe (U.Vector DNA)
textToDNA = toDNA . U.fromList

{- | A function that filters the string so that only letters and spaces remain, then
splits the result on every space so that only words remain.
-}
textToWords :: String -> V.Vector String
textToWords x = V.fromList $ words $ map toLower $ filter (\a -> isAlphaNum a || isSpace a) x

{- | A function that filters the string so that only letters and spaces remain, then
splits the result on every space so that only words remain. It remembers the original
start and end index of each word.
-}
textToWordsWithIndices :: String -> V.Vector ((Int, Int), String)
textToWordsWithIndices input = V.fromList $ map toWord $ wordsWithIndices indexedCharacters
  where
    indexedCharacters :: [(Int, Char)]
    indexedCharacters = U.toList $ filterSpaceAndLetters input

    -- Convert a list of indexed characters to an indexed string
    toWord :: [(Int, Char)] -> ((Int, Int), [Char])
    toWord [] = error "Empty string"
    toWord word@(firstIndexedChar : _) =
        ( (fst firstIndexedChar, fst (last word) + 1)
        , map snd word
        )

    -- The words function as written in Prelude, but on indexed characters
    wordsWithIndices :: [(Int, Char)] -> [[(Int, Char)]]
    wordsWithIndices s
        | null checking = []
        | otherwise = word : wordsWithIndices remaining
      where
        checking = dropWhile (isSpace . snd) s
        (word, remaining) = break (isSpace . snd) checking

    filterSpaceAndLetters :: String -> U.Vector (Int, Char)
    filterSpaceAndLetters w =
        U.filter
            (\x -> (isAlphaNum . snd) x || (isSpace . snd) x)
            (U.indexed (U.fromList $ map toLower w))

-- If trying to parse the string to DNA would fail, throw a more readable error
tryParseDNA :: String -> U.Vector DNA
-- tryParseDNA input
--     | (isNothing . parseDna) input = error "Invalid DNA string"
--     | otherwise = (fromJust . parseDna) input
tryParseDNA input = fromMaybe (error "Invalid DNA string") (parseDna input)
parseDna :: String -> Maybe (U.Vector DNA)
parseDna = textToDNA . U.toList . filterLetters
