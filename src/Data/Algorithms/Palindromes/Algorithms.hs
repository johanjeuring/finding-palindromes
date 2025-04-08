{-# LANGUAGE MonoLocalBinds #-}

{- |
Module      :  Data.Algorithms.Palindromes.Algorithms
Copyright   :  (c) 2007 - 2013 Johan Jeuring
License     :  BSD3
Maintainer  :  johan@jeuring.net
Stability   :  experimental
Portability :  portable

This program has been developed by students from the bachelor Computer Science at Utrecht
University within the Software Project course.
© Copyright Utrecht University (Department of Information and Computing Sciences)

This module Contains two functions that run algorithms for finding palindromes.
One runs a linear time algorithms and the other a quadratic algorithm.
These assume text has been preprocessed.
They return the max length of the palindrome at each center index.
-}
module Data.Algorithms.Palindromes.Algorithms
    ( linearAlgorithm
    , quadraticAlgorithm
    ) where

import Data.Algorithms.Palindromes.LinearAlgorithm (extendPalindromeS)
import Data.Algorithms.Palindromes.PalEq (PalEq)
import Data.Algorithms.Palindromes.QuadraticAlgorithm
    ( gappedApproximatePalindromesAroundCentres
    )

import qualified Data.Vector as V

{-- | Search for palindromes using the linear time algorithm.
Returns a list of a list of the maximum length palindrome that was found at each center index in the input.
-}
linearAlgorithm
    :: (PalEq a)
    => Bool
    -- ^ isAntiReflexive, antireflexive types only need to check even indices
    -> V.Vector a
    -> [Int]
linearAlgorithm isAntiReflexive input = reverse $ extendPalindromeS isAntiReflexive input 0 [] 0

{-- | Search for palindromes using the quadratic algorithm.
Returns a list of a list of the maximum length palindrome that was found at each center index in the input.
Gaps allow the palindrome to have a gap at the center of given length, errors allow n mistakes in the palindrome of given Int.
-}
quadraticAlgorithm
    :: (PalEq a)
    => Bool
    -- ^ isAntiReflexive
    -> Int
    -- ^ gapsize
    -> Int
    -- ^ error count
    -> V.Vector a
    -- ^ input
    -> [Int]
quadraticAlgorithm = gappedApproximatePalindromesAroundCentres