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

{- | A function that filters the string so that only letters and spaces remain, then
splits the result on every space so that only words remain. It remembers the original
start and end index of each word.
-}
textToWordsWithIndices :: U.Vector Char -> V.Vector ((Int, Int), String)
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

    filterSpaceAndLetters :: U.Vector Char -> U.Vector (Int, Char)
    filterSpaceAndLetters w =
        U.filter
            (\x -> (isAlphaNum . snd) x || (isSpace . snd) x)
            (U.indexed (U.map toLower w))

-- If trying to parse the string to DNA would fail, throw a more readable error
tryParseDNA :: U.Vector Char -> U.Vector DNA
-- tryParseDNA input
--     | (isNothing . parseDna) input = error "Invalid DNA string"
--     | otherwise = (fromJust . parseDna) input
tryParseDNA input = fromMaybe (error "Invalid DNA string") (parseDna input)
parseDna :: U.Vector Char -> Maybe (U.Vector DNA)
parseDna = toDNA . filterLetters
