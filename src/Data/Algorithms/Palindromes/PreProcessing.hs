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
import Data.Maybe (fromJust, isNothing)

import Data.Algorithms.Palindromes.DNA
    ( DNA
    , toDNA
    )

import qualified Data.Vector as V

-- Make sure all functions are of the type
-- (PalEq b) => String -> [b]

-- | A function that filters the string so that only letters remain
filterLetters :: String -> V.Vector Char
filterLetters x = V.map toLower $ V.filter isAlphaNum (V.fromList x)

{- | A function that filters the string so that only letters remain, but remembers the
original index of each character.
-}
filterLetters' :: String -> V.Vector (Int, Char)
filterLetters' x = V.filter (isAlphaNum . snd) (V.indexed (V.fromList $ map toLower x))

-- | A function that parses ATGCN to the DNA datatype
textToDNA :: String -> Maybe (V.Vector DNA)
textToDNA = toDNA . V.fromList

{- | A function that filters the string so that only letters and spaces remain, then
splits the result on every space so that only words remain.
-}
textToWords :: String -> V.Vector String
textToWords x = V.fromList $ words $ map toLower $ filter (\a -> isAlphaNum a || isSpace a) x

{- | A function that filters the string so that only letters and spaces remain, then
splits the result on every space so that only words remain. It remembers the original
start and end index of each word.
-}
textToWordsWithIndices :: String -> V.Vector ((Int, Int), [Char])
textToWordsWithIndices input = V.fromList $ map toWord $ wordsWithIndices indexedCharacters
  where
    indexedCharacters :: [(Int, Char)]
    indexedCharacters = V.toList $ filterSpaceAndLetters input

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

    filterSpaceAndLetters :: String -> V.Vector (Int, Char)
    filterSpaceAndLetters w =
        V.filter
            (\x -> (isAlphaNum . snd) x || (isSpace . snd) x)
            (V.indexed (V.fromList $ map toLower w))

-- If trying to parse the string to DNA would fail, throw a more readable error
tryParseDNA :: String -> V.Vector DNA
tryParseDNA input
    | (isNothing . parseDna) input = error "Invalid DNA string"
    | otherwise = (fromJust . parseDna) input
parseDna :: String -> Maybe (V.Vector DNA)
parseDna = textToDNA . V.toList . filterLetters
