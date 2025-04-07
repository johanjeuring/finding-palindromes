{- |
Module      :  Data.Algorithms.Palindromes.PreProcessing
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

Functions for converting string input to different variants that can be used in the algorithms.
-}
module Data.Algorithms.Palindromes.PreProcessing
    ( filterLetters
    , filterLetters'
    , textToDNA
    , textToWords
    ) where

import Data.Algorithms.Palindromes.DNA
    ( DNA
    , toDNA
    )
import Data.Char (isAlphaNum, isSpace, toLower)

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