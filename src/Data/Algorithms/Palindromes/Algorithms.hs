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

This module exports three functions that run algorithms for finding palindromes.
One runs a linear time algorithms and the other a quadratic algorithm.
These assume text has been preprocessed.
-}
module Data.Algorithms.Palindromes.Algorithms
    ( linearAlgorithm
    , quadraticAlgorithm
    , approximateAlgorithm
    ) where

import Data.Algorithms.Palindromes.Internal.ApproximateAlgorithm (approximateAlgorithm)
import Data.Algorithms.Palindromes.Internal.LinearAlgorithm (extendPalindromeS)
import Data.Algorithms.Palindromes.Internal.QuadraticAlgorithm
    ( gappedApproximatePalindromesAroundCentres
    )
import Data.Algorithms.Palindromes.PalEq (PalEq)

import qualified Data.Vector.Generic as G

{- | Search for palindromes using the linear time algorithm. Returns a list of the maximum
length palindromes which were found at each center index in the input.
-}
linearAlgorithm
    :: (PalEq a, G.Vector v a)
    => Bool
    -- ^ isAntiReflexive, antireflexive types only need to check even indices
    -> v a
    -> [Int]
linearAlgorithm isAntiReflexive input =
    reverse $
        extendPalindromeS isAntiReflexive input 0 [] 0

{- | Search for palindromes using the quadratic algorithm. Returns a list of the maximum
length palindromes which were found at each center index in the input. Gaps allow the
palindrome to have a gap at the center of given length. Errors allow some substitution
mistakes in the palindrome.
-}
quadraticAlgorithm
    :: (PalEq a, G.Vector v a)
    => Bool
    -- ^ isAntiReflexive
    -> Int
    -- ^ gapSize
    -> Int
    -- ^ maxErrors
    -> v a
    -- ^ input
    -> [Int]
quadraticAlgorithm = gappedApproximatePalindromesAroundCentres
