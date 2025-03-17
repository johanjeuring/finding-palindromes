-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- \|
-- Module      :  Data.Algorithms.Palindromes.Algorithms
-- Copyright   :  (c) 2007 - 2013 Johan Jeuring
-- License     :  BSD3
--
-- Maintainer  :  johan@jeuring.net
-- Stability   :  experimental
-- Portability :  portable

module Data.Algorithms.Palindromes.Algorithms
    ( linearAlgorithm
    , quadraticAlgorithm
    ) where

import Data.Algorithms.Palindromes.LinearAlgorithm (extendPalindromeS)
import Data.Algorithms.Palindromes.PalindromesUtils (Couplable)
import Data.Algorithms.Palindromes.QuadraticAlgorithm
    ( gappedApproximatePalindromesAroundCentres
    )

import qualified Data.Vector as V

-- | Search for palindromes using the linear algorithm
linearAlgorithm
    :: (Couplable a)
    => Bool
    -- ^ isAntiReflexive
    -> V.Vector a
    -> [Int]
linearAlgorithm isAntiReflexive input = reverse $ extendPalindromeS isAntiReflexive input 0 [] 0

-- appendseq (list, s) = toList s ++ list

-- | Search for palindromes using the quadratic algorithm
quadraticAlgorithm
    :: (Couplable a)
    => Bool -- isAntiReflexive
    -> Int -- gapsize
    -> Int -- error count
    -> V.Vector a -- input
    -> [Int]
quadraticAlgorithm = gappedApproximatePalindromesAroundCentres