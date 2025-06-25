{-# LANGUAGE MonoLocalBinds #-}

{- |
Module      :  Data.Algorithms.Palindromes.Algorithms
Copyright   :  (c) 2007 - 2025 Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  provisional
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences) and Johan Jeuring

This module exports three functions that run algorithms for finding palindromes in Vectors:
1. The linear algorithm (for finding palindromes in linear time).
2. The quadratic algorithm (for finding palindromes in quadratic time).
3. The Approximate Palindrome algorithm for finding approximate palindromes. This algorithm also runs in quadratic time

More information about what features the algorithms support is in the README.md.
-}
module Data.Algorithms.Palindromes.Algorithms
    ( linearAlgorithm
    , quadraticAlgorithm
    , approximateAlgorithm
    ) where

import Data.Algorithms.Palindromes.Internal.ApproximateAlgorithm (approximateAlgorithm)
import Data.Algorithms.Palindromes.Internal.LinearAlgorithm (extendPalindromeS)
import Data.Algorithms.Palindromes.Internal.QuadraticAlgorithm
    ( maxPalindromePerCenter
    )
import Data.Algorithms.Palindromes.PalEq (PalEq)

import qualified Data.Vector.Generic as G

{- | Search for palindromes using the linear time algorithm. Returns a list of lengths of
the maximal palindromes which were found at each center from the input.
-}
linearAlgorithm
    :: (PalEq a, G.Vector v a)
    => Bool
    {- ^ Represents if the datatype 'a' is anti-reflexive. For anti-reflexive datatypes we
    only need to check even indices, because the linear algorithm does not support gaps.
    -}
    -> v a
    -- ^ The input vector to find palindromes in
    -> [Int]
    -- ^ A list of integers representing the palindrome lengths at every center position.
linearAlgorithm isAntiReflexive input =
    reverse $
        extendPalindromeS isAntiReflexive input 0 [] 0

{- | Search for palindromes using the quadratic algorithm. Returns a list of lengths of
the maximal palindromes which were found at each center from the input. Gaps allow the
palindrome to have a gap at the center of given length. Errors allow some substitution
errors in the palindrome.
-}
quadraticAlgorithm
    :: (PalEq a, G.Vector v a)
    => Bool
    {- ^ Represents whether we only need to search for even palindromes. This is the case
    if we have an even-sized gap and an anti-reflexive datatype.
    -}
    -> Int
    {- ^ Represents the size of a gap in the center when looking for palindromes.
    This means that the middle X characters will be ignored at every center.
    -}
    -> Int
    -- ^ Represents that maximum allowed substitution errors when looking for palindromes.
    -> v a
    -- ^ The input vector to find palindromes in.
    -> [Int]
    -- ^ A list of integers representing the palindrome lengths at every center position.
quadraticAlgorithm = maxPalindromePerCenter
