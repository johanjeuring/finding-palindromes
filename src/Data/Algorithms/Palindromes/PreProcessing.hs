-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- \|
-- Module      :  Data.Algorithms.Palindromes.PreProcessing
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable

module Data.Algorithms.Palindromes.PreProcessing
    ( filterLetters
    , filterLetters'
    , textToDNA
    , textToWords
    , textToWords'
    ) where

import Data.Algorithms.Palindromes.DNA
    ( DNA
    , toDNA
    )
import Data.Char (isAlphaNum, isLetter, isSpace, toLower)

import qualified Data.Vector as V

-- Make sure all functions are of the type
-- (PalEq b) => String -> [b]

-- | A function that filters the string so that only letters remain
filterLetters :: String -> V.Vector Char
filterLetters x = V.map toLower $ V.filter isAlphaNum (V.fromList x)

-- | A function that filters the string so that only letters remain, but remembers the original index of each character.
filterLetters' :: String -> V.Vector (Int, Char)
filterLetters' x = V.filter (isAlphaNum . snd) (V.indexed (V.fromList $ map toLower x))

-- | A function that parses ATGCN to the DNA datatype
textToDNA :: String -> Maybe (V.Vector DNA)
textToDNA = toDNA . V.fromList

-- | A function that filters the string so that only letters and spaces remain, then splits the result on every space so that only words remain.
textToWords :: String -> V.Vector String
textToWords x = V.fromList $ words $ map toLower $ filter (\a -> isAlphaNum a || isSpace a) x

-- | A function that filters the string so that only letters and spaces remain, then splits the result on every space so that only words remain. It remembers the original start and end index of each word.
textToWords' :: String -> V.Vector ((Int, Int), [Char])
textToWords' x = V.fromList $ map toWord $ words' indexedCharacters
  where
    indexedCharacters :: [(Int, Char)]
    indexedCharacters = V.toList $ filterSpaceAndLetters x

    -- Convert a list of indexed characters to an indexed string
    toWord :: [(Int, Char)] -> ((Int, Int), [Char])
    toWord word@(firstIndexedChar : _) =
        ( (fst firstIndexedChar, fst (last word) + 1)
        , map snd word
        )

    -- The words function as written in Prelude, but on indexed characters
    words' :: [(Int, Char)] -> [[(Int, Char)]]
    words' s
        | null checking = []
        | otherwise = word : words' remaining
      where
        checking = dropWhile (isSpace . snd) s
        (word, remaining) = break (isSpace . snd) checking

    filterSpaceAndLetters :: String -> V.Vector (Int, Char)
    filterSpaceAndLetters x =
        V.filter
            (\x -> (isAlphaNum . snd) x || (isSpace . snd) x)
            (V.indexed (V.fromList $ map toLower x))
