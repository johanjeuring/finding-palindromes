{- |
Module      :  Data.Algorithms.Palindromes.DNA
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

This module contains the DNA datatype and corresponding functions.
This type encodes a representation for DNA that can be used as input for finding palindromes.
-}
module Data.Algorithms.Palindromes.DNA where

import Data.Algorithms.Palindromes.PalEq (PalEq ((=:=)))
import Data.Char (toUpper)
import Data.Maybe (fromJust, isNothing)

{- | Datatype for the different DNA, note that (=)/Eq is not suitable for checking if DNA
  has palindromes, instead PalEq should be used.
-}
data DNA = A | T | C | G | N deriving (Show, Eq)

-- | Declare instance PalEq for DNA. A and T form a couple, C and G form a couple.
instance {-# OVERLAPPING #-} PalEq DNA where
    A =:= T = True
    T =:= A = True
    G =:= C = True
    C =:= G = True
    _ =:= _ = False

{- | Parsed a foldable with chars to a foldable with the DNA datatype. Returns Nothing if
  the input cannot be fully parsed to DNA.
-}
toDNA :: (Functor f, Foldable f) => f Char -> Maybe (f DNA)
toDNA x = if hasNothing then Nothing else Just $ fmap (fromJust . charToDNA) x
  where
    hasNothing = any (isNothing . charToDNA) x

-- | Parses a single Char to the DNA datatype. Returns Nothing if this is not possible.
charToDNA :: Char -> Maybe DNA
charToDNA 'A' = Just A
charToDNA 'T' = Just T
charToDNA 'C' = Just C
charToDNA 'G' = Just G
charToDNA 'U' = Just T
charToDNA 'a' = Just A
charToDNA 't' = Just T
charToDNA 'c' = Just C
charToDNA 'g' = Just G
charToDNA 'u' = Just T
charToDNA c = if toUpper c `elem` "RYKMSWBDHVN" then Just N else Nothing

-- | Converts the DNA datatype to the corresponding Char symbol
dnaToChar :: DNA -> Char
dnaToChar A = 'A'
dnaToChar T = 'T'
dnaToChar G = 'G'
dnaToChar C = 'C'
dnaToChar N = 'N'